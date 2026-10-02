{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Talking to Wise.
--
-- The only module in this package that does network IO, so that everything
-- deciding what a ledger says stays a pure function over data a test can hold.
--
-- Statement endpoints are protected by Strong Customer Authentication: the
-- first request comes back 403 carrying a one-time token, which has to be
-- signed with a private key whose public half is registered on the Wise
-- account, and the same request made again with the signature.  That exchange
-- is what 'fetchSigned' does, and it is the reason this importer wants a key
-- file as well as an API token.
module Centjes.Import.Wise.API
  ( WiseApiToken (..),
    WiseSigningKey,
    WiseConnection (..),
    readSigningKey,
    Profile (..),
    Balance (..),
    WiseApiError (..),
    renderWiseApiError,
    WiseM,
    runWiseM,
    fetchProfiles,
    fetchBalances,
    fetchStatementCsv,
    statementUrl,
  )
where

import Autodocodec
import Centjes.CurrencySymbol (CurrencySymbol (..))
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import qualified Data.ByteString as SB
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.X509 (PrivKey (..))
import Data.X509.File (readKeyFile)
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Path

-- | A Wise API token.
--
-- Deliberately has no 'Show' instance, so that a token cannot reach a log line
-- or a crash report by accident.  One that does has to be rotated.
newtype WiseApiToken = WiseApiToken {unWiseApiToken :: Text}

-- | The private key that answers Wise's authentication challenge.
--
-- No 'Show' instance, for the same reason as the token: this is the secret that
-- lets a token read statements at all.
newtype WiseSigningKey = WiseSigningKey RSA.PrivateKey

-- | Everything a request to Wise needs besides its URL.
data WiseConnection = WiseConnection
  { wiseConnectionManager :: !Manager,
    wiseConnectionToken :: !WiseApiToken,
    -- | Absent when no key was configured, which is fine right up until Wise
    -- asks for a signature.
    wiseConnectionSigningKey :: !(Maybe WiseSigningKey),
    wiseConnectionBaseUrl :: !String
  }

data WiseApiError
  = WiseApiErrorHttp !String !HttpException
  | WiseApiErrorStatus !String !Int !LB.ByteString
  | WiseApiErrorDecode !String !String
  | WiseApiErrorKeyUnreadable !FilePath !String
  | WiseApiErrorKeyNotRSA !FilePath
  | WiseApiErrorSigningFailed !String
  | WiseApiErrorChallengeWithoutKey !String
  | WiseApiErrorChallengeRefused !String

renderWiseApiError :: WiseApiError -> String
renderWiseApiError = \case
  -- Showing an 'HttpException' shows the request that failed, headers and all.
  -- That is only safe because http-client's own 'Show' redacts the
  -- Authorization header, which is where the token lives.  Anything that
  -- formats the request itself has to redact it.
  WiseApiErrorHttp url e -> unwords ["The request to", url, "failed:", show e]
  WiseApiErrorStatus url status body ->
    unlines $
      concat
        [ [ unwords ["The request to", url, "returned status", show status <> ":"],
            T.unpack (TE.decodeUtf8Lenient (LB.toStrict body))
          ],
          [ unwords
              [ "Wise does not offer statements over the API to every country.",
                "If this account is not in one of the countries it does,",
                "export the statement from the website and run the import on the file."
              ]
          | status == 403 || status == 404
          ]
        ]
  WiseApiErrorDecode url reason ->
    unlines
      [ unwords ["The response from", url, "is not what this importer expects:"],
        reason
      ]
  WiseApiErrorKeyUnreadable fp reason ->
    unlines [unwords ["Could not read a private key from", fp <> ":"], reason]
  WiseApiErrorKeyNotRSA fp ->
    unwords
      [ "The key in",
        fp,
        "is not an RSA key.  Wise signs its authentication challenges with RSA,",
        "so the key registered with Wise has to be one."
      ]
  WiseApiErrorSigningFailed reason ->
    unwords ["Could not sign Wise's authentication challenge:", reason]
  WiseApiErrorChallengeWithoutKey url ->
    unlines
      [ unwords ["Wise asked for a signature before answering", url <> ",", "and no key was configured."],
        unwords
          [ "Generate a key pair, upload the public half in your Wise account settings,",
            "and point --private-key-file at the private half."
          ]
      ]
  WiseApiErrorChallengeRefused url ->
    unlines
      [ unwords ["Wise refused the signature for", url <> "."],
        unwords
          [ "The key this importer signed with is probably not the one registered",
            "on the account."
          ]
      ]

type WiseM = ExceptT WiseApiError (LoggingT IO)

runWiseM :: WiseM a -> LoggingT IO (Either WiseApiError a)
runWiseM = runExceptT

-- | A profile, which is what a Wise account's balances hang off.
data Profile = Profile
  { profileId :: !Int64,
    -- | @PERSONAL@ or @BUSINESS@.
    profileType :: !Text
  }

instance HasCodec Profile where
  codec =
    object "Profile" $
      Profile
        <$> requiredField "id" "profile id" .= profileId
        <*> requiredField "type" "whether this is a personal or a business profile" .= profileType

-- | One currency's balance within a profile.
data Balance = Balance
  { balanceId :: !Int64,
    balanceCurrency :: !CurrencySymbol
  }

instance HasCodec Balance where
  codec =
    object "Balance" $
      Balance
        <$> requiredField "id" "balance id" .= balanceId
        <*> requiredField "currency" "the currency this balance is held in" .= balanceCurrency

readSigningKey :: Path Abs File -> IO (Either WiseApiError WiseSigningKey)
readSigningKey keyFile = do
  let fp = fromAbsFile keyFile
  errOrKeys <- try (readKeyFile fp)
  pure $ case errOrKeys of
    Left (e :: IOError) -> Left $ WiseApiErrorKeyUnreadable fp (show e)
    Right keys -> case [k | PrivKeyRSA k <- keys] of
      (k : _) -> Right (WiseSigningKey k)
      [] ->
        if null keys
          then Left $ WiseApiErrorKeyUnreadable fp "The file holds no private key."
          else Left $ WiseApiErrorKeyNotRSA fp

fetchProfiles :: WiseConnection -> WiseM [Profile]
fetchProfiles connection = do
  let url = wiseConnectionBaseUrl connection <> "/v2/profiles"
  body <- fetchSigned connection url
  decodeBody url body

fetchBalances :: WiseConnection -> Int64 -> WiseM [Balance]
fetchBalances connection profile = do
  let url = wiseConnectionBaseUrl connection <> "/v4/profiles/" <> show profile <> "/balances?types=STANDARD"
  body <- fetchSigned connection url
  decodeBody url body

-- | Download one balance's statement as CSV.
fetchStatementCsv ::
  WiseConnection ->
  Int64 ->
  Balance ->
  UTCTime ->
  UTCTime ->
  WiseM SB.ByteString
fetchStatementCsv connection profile balance begin end = do
  let url = statementUrl (wiseConnectionBaseUrl connection) profile balance begin end
  logInfoN $
    T.pack $
      unwords
        [ "Fetching the",
          T.unpack (currencySymbolText (balanceCurrency balance)),
          "statement"
        ]
  LB.toStrict <$> fetchSigned connection url

-- | Where one balance's statement lives.
--
-- @COMPACT@ is the type with one row per transaction, which is the shape this
-- importer reads: the @FLAT@ type splits a fee onto a row of its own, which
-- would have to be matched back up to the row it belongs to.
statementUrl :: String -> Int64 -> Balance -> UTCTime -> UTCTime -> String
statementUrl baseUrl profile balance begin end =
  concat
    [ baseUrl,
      "/v1/profiles/",
      show profile,
      "/balance-statements/",
      show (balanceId balance),
      "/statement.csv?",
      T.unpack $
        TE.decodeUtf8Lenient $
          HTTP.renderSimpleQuery
            False
            [ ("currency", TE.encodeUtf8 (currencySymbolText (balanceCurrency balance))),
              ("intervalStart", apiTime begin),
              ("intervalEnd", apiTime end),
              ("type", "COMPACT")
            ]
    ]

apiTime :: UTCTime -> SB.ByteString
apiTime = TE.encodeUtf8 . T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ"

-- | Make a request, answering an authentication challenge if one comes back.
--
-- Wise replies 403 with a one-time token in @x-2fa-approval@ rather than
-- failing outright; the same request signed with the registered key is then
-- allowed through.  Only the challenge is signed, so nothing about the request
-- itself has to be reproduced byte for byte.
fetchSigned :: WiseConnection -> String -> WiseM LB.ByteString
fetchSigned connection url = do
  let manager = wiseConnectionManager connection
  initialRequest <- parseWiseRequest (wiseConnectionToken connection) url
  response <- performRequest manager url initialRequest
  case challengeToken response of
    Nothing -> responseBodyOrError url response
    Just oneTimeToken -> case wiseConnectionSigningKey connection of
      Nothing -> throwE $ WiseApiErrorChallengeWithoutKey url
      Just key -> do
        signature <- either throwE pure (signChallenge key oneTimeToken)
        let signedRequest =
              initialRequest
                { requestHeaders =
                    requestHeaders initialRequest
                      ++ [ ("X-2FA-Approval", oneTimeToken),
                           ("X-Signature", signature)
                         ]
                }
        signedResponse <- performRequest manager url signedRequest
        case challengeToken signedResponse of
          Just _ -> throwE $ WiseApiErrorChallengeRefused url
          Nothing -> responseBodyOrError url signedResponse

-- | The one-time token Wise wants signed, if this response is a challenge.
challengeToken :: Response LB.ByteString -> Maybe SB.ByteString
challengeToken response =
  if HTTP.statusCode (responseStatus response) == 403
    then lookup "x-2fa-approval" (responseHeaders response)
    else Nothing

signChallenge :: WiseSigningKey -> SB.ByteString -> Either WiseApiError SB.ByteString
signChallenge (WiseSigningKey key) oneTimeToken =
  case PKCS15.sign Nothing (Just SHA256) key oneTimeToken of
    Left e -> Left $ WiseApiErrorSigningFailed (show e)
    Right signature -> Right (Base64.encode signature)

parseWiseRequest :: WiseApiToken -> String -> WiseM Request
parseWiseRequest token url = do
  request <- case parseRequest url of
    Nothing -> throwE $ WiseApiErrorDecode url "Not a URL this importer can request."
    Just r -> pure r
  -- No Accept header: the last part of the path is what says which format Wise
  -- answers in, and asking for JSON while asking for statement.csv is a way to
  -- be surprised.
  pure
    request
      { requestHeaders = [("Authorization", TE.encodeUtf8 ("Bearer " <> unWiseApiToken token))]
      }

performRequest :: Manager -> String -> Request -> WiseM (Response LB.ByteString)
performRequest manager url request = do
  errOrResponse <- liftIO $ try $ httpLbs request manager
  case errOrResponse of
    Left e -> throwE $ WiseApiErrorHttp url e
    Right response -> pure response

responseBodyOrError :: String -> Response LB.ByteString -> WiseM LB.ByteString
responseBodyOrError url response =
  let status = HTTP.statusCode (responseStatus response)
   in if status >= 200 && status < 300
        then pure (responseBody response)
        else throwE $ WiseApiErrorStatus url status (responseBody response)

decodeBody :: (HasCodec a) => String -> LB.ByteString -> WiseM a
decodeBody url body = case eitherDecodeJSONViaCodec body of
  Left err -> throwE $ WiseApiErrorDecode url err
  Right a -> pure a
