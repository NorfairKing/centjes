{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Format (runCentjesFormat) where

import Centjes.Compile
import Centjes.DecimalLiteral
import Centjes.Format
import Centjes.Load
import Centjes.Module
import Centjes.OptParse
import Centjes.Parse
import Centjes.Validation
import Control.Arrow (second)
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Either
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Money.QuantisationFactor
import Path
import Path.IO
import System.Exit
import UnliftIO

runCentjesFormat :: Settings -> FormatSettings -> IO ()
runCentjesFormat Settings {..} FormatSettings {..} = do
  runStderrLoggingT $ do
    case formatSettingFileOrDir of
      Nothing -> formatFromLedger settingLedgerFile
      Just (Right dir) -> formatDir M.empty dir
      Just (Left file) -> formatSingleFile file

formatFromLedger :: Path Abs File -> LoggingT IO ()
formatFromLedger l = do
  -- TODO find a way to not read and parse every file twice (worst-case)
  currencies <- do
    ds <- loadModules l
    case compileCurrencies ds of
      Failure errs -> do
        logWarnN $
          T.pack $
            unlines $
              "Could not compile currency declarations:"
                : map displayException (NE.toList errs)
        pure M.empty
      Success m -> pure m
  logDebugN $ T.pack $ unlines $ "Compiled currencies:" : map show (M.toList currencies)
  formatDir currencies (parent l)

formatDir :: Map CurrencySymbol QuantisationFactor -> Path Abs Dir -> LoggingT IO ()
formatDir currencies d = do
  files <- snd <$> listDirRecur d

  let pen :: (a, Either b c) -> Either (a, b) (a, c)
      pen (a, Left b) = Left (a, b)
      pen (a, Right c) = Right (a, c)

  parsedFiles <- fmap catMaybes $ forConcurrently files $ \fp ->
    if fileExtension fp == Just ".cent"
      then Just . pen . (,) fp . fmap (second (fillInDigits currencies)) <$> parseFile fp
      else pure Nothing

  parsedModules <- case partitionEithers parsedFiles of
    ([], ms) -> pure ms
    (errs, _) -> do
      logErrorN $
        T.pack $
          unlines $
            flip map errs $ \(fp, err) ->
              concat
                [ unwords ["Failed to parse", fromAbsFile fp],
                  "\n",
                  err
                ]
      liftIO $ die "Could not parse all files. Not continuing to formatting them."

  let checkedModules = map (\(fp, (textContents, m)) -> pen ((fp, textContents), idempotenceTest fp m)) parsedModules

  readyToFormatModules <- case partitionEithers checkedModules of
    ([], ms) -> pure ms
    (errs, _) -> do
      logErrorN $
        T.pack $
          unlines $
            flip map errs $ \((fp, _), err) ->
              concat
                [ unwords ["Idempotence test failed for", fromAbsFile fp],
                  "\n",
                  err
                ]
      liftIO $ die "At least one file failed the idempotence test. Not continuing to formatting them."

  forConcurrently_ readyToFormatModules $ \((fp, textContents), newTextContents) -> do
    formatFile fp textContents newTextContents

-- Format a single file on its own.
-- This ignores any declarations.
formatSingleFile :: Path Abs File -> LoggingT IO ()
formatSingleFile fp = do
  errOrModule <- parseFile fp
  case errOrModule of
    Left err ->
      logWarnN $
        T.pack $
          unlines
            [ "Could not format file because it does not look like Utf-8: ",
              show fp,
              show err
            ]
    Right (textContents, m) -> do
      newTextContents <- case idempotenceTest fp m of
        Left err -> liftIO $ die err
        Right ft -> pure ft
      formatFile fp textContents newTextContents

parseFile :: Path Abs File -> LoggingT IO (Either String (Text, Module))
parseFile fp = do
  contents <- liftIO $ SB.readFile (fromAbsFile fp)
  case TE.decodeUtf8' contents of
    Left err -> pure (Left (show err))
    Right textContents ->
      pure $ case parseModule (fromAbsFile fp) textContents of
        Left err -> Left err
        Right m -> Right (textContents, m)

formatFile :: Path Abs File -> Text -> Text -> LoggingT IO ()
formatFile fp textContents newTextContents =
  if newTextContents == textContents
    then logDebugN $ T.pack $ unwords ["Did not format because nothing changed:", fromAbsFile fp]
    else do
      liftIO $ SB.writeFile (fromAbsFile fp) $ TE.encodeUtf8 newTextContents
      logInfoN $ T.pack $ unwords ["Formatted", fromAbsFile fp]

idempotenceTest :: Path Abs File -> Module -> Either String Text
idempotenceTest fp m = do
  let newTextContents = formatModule m
  case parseModule "idempotence-test" newTextContents of
    Left err ->
      Left $
        unlines
          [ "Formatted file could not be parsed, this indicates a bug:",
            err,
            T.unpack newTextContents
          ]
    Right m' ->
      if newTextContents == formatModule m'
        then Right newTextContents
        else
          Left $
            unlines
              [ "Formatting was not idempotent.",
                fromAbsFile fp,
                "Before:",
                show m,
                "After:",
                show m'
              ]

fillInDigits :: Map CurrencySymbol QuantisationFactor -> Module -> Module
fillInDigits currencies m = m {moduleDeclarations = map goDeclaration (moduleDeclarations m)}
  where
    goDeclaration :: Declaration -> Declaration
    goDeclaration = \case
      DeclarationTransaction t -> DeclarationTransaction $ goTransaction t
      d -> d
    goTransaction :: Transaction -> Transaction
    goTransaction t = t {transactionPostings = map goPosting (transactionPostings t)}
    goPosting :: Posting -> Posting
    goPosting p = case M.lookup (postingCurrencySymbol p) currencies of
      Nothing -> p
      Just qf -> p {postingAccount = (postingAccount p) {decimalLiteralDigits = quantisationFactorDigits qf}}
