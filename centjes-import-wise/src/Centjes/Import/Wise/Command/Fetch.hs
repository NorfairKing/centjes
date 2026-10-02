{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Wise.Command.Fetch
  ( runCentjesImportWiseFetch,
    statementFileName,
  )
where

import Centjes.CurrencySymbol (currencySymbolText)
import Centjes.Import.Wise.API
import Centjes.Import.Wise.OptParse
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Client.TLS (newTlsManager)
import Path
import Path.IO
import System.Exit

runCentjesImportWiseFetch :: FetchSettings -> IO ()
runCentjesImportWiseFetch FetchSettings {..} = do
  today <- utctDay <$> getCurrentTime
  let begin = fromMaybe (startOfYear today) fetchSettingBegin
  let end = fromMaybe today fetchSettingEnd
  unless (begin <= end) $
    die $
      unwords
        [ "The statement would begin on",
          formatTime defaultTimeLocale "%F" begin,
          "and end on",
          formatTime defaultTimeLocale "%F" end <> ",",
          "which is before it begins."
        ]

  mKey <- traverse readSigningKeyOrDie fetchSettingPrivateKeyFile
  ensureDir fetchSettingOutputDirectory
  manager <- newTlsManager

  errOrDone <- runStderrLoggingT $ runWiseM $ do
    profiles <- fetchProfiles manager fetchSettingToken fetchSettingBaseUrl
    let wanted = case fetchSettingProfile of
          Nothing -> profiles
          Just only -> filter ((== only) . profileId) profiles
    mapM_ (fetchProfileStatements manager mKey begin end) wanted

  case errOrDone of
    Left err -> die $ renderWiseApiError err
    Right () -> pure ()
  where
    fetchProfileStatements manager mKey begin end profile = do
      balances <- fetchBalances manager fetchSettingToken fetchSettingBaseUrl (profileId profile)
      mapM_ (fetchBalanceStatement manager mKey begin end profile) balances

    fetchBalanceStatement manager mKey begin end profile balance = do
      contents <-
        fetchStatementCsv
          manager
          fetchSettingToken
          mKey
          fetchSettingBaseUrl
          (profileId profile)
          balance
          (startOfDay begin)
          (endOfDay end)
      fileName <- either (liftIO . die) pure (statementFileName (profileId profile) balance)
      let statementFile = fetchSettingOutputDirectory </> fileName
      liftIO $ SB.writeFile (fromAbsFile statementFile) contents
      logInfoN $ T.pack $ unwords ["Wrote", fromAbsFile statementFile]

readSigningKeyOrDie :: Path Abs File -> IO WiseSigningKey
readSigningKeyOrDie keyFile = do
  errOrKey <- readSigningKey keyFile
  case errOrKey of
    Left err -> die $ renderWiseApiError err
    Right key -> pure key

-- | What to call the file a balance's statement is downloaded into.
--
-- The profile is in the name as well as the currency, because one token can
-- reach a personal and a business profile and both can hold the same currency.
statementFileName :: Int64 -> Balance -> Either String (Path Rel File)
statementFileName profile balance =
  let name =
        concat
          [ "wise-",
            show profile,
            "-",
            T.unpack (currencySymbolText (balanceCurrency balance)),
            ".csv"
          ]
   in case parseRelFile name of
        Nothing -> Left $ unwords ["Not a file name this importer can write:", show name]
        Just relFile -> Right relFile

startOfYear :: Day -> Day
startOfYear day =
  let (year, _, _) = toGregorian day
   in fromGregorian year 1 1

startOfDay :: Day -> UTCTime
startOfDay day = UTCTime day 0

-- | The last instant of a day, so that a statement asked for up to a day
-- includes what happened on it.
endOfDay :: Day -> UTCTime
endOfDay day = UTCTime day (86400 - 0.001)
