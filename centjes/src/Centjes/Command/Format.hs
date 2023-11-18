{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Format (runCentjesFormat) where

import Centjes.Format
import Centjes.Module
import Centjes.OptParse
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import System.Exit

runCentjesFormat :: Settings -> FormatSettings -> IO ()
runCentjesFormat Settings {..} FormatSettings {..} = do
  runStderrLoggingT $ do
    case formatSettingFileOrDir of
      Nothing -> formatDir (parent settingLedgerFile)
      Just (Right dir) -> formatDir dir
      Just (Left file) -> formatSingleFile file

formatDir :: Path Abs Dir -> LoggingT IO ()
formatDir d = do
  files <- snd <$> listDirRecur d

  let pen :: (a, Either b c) -> Either (a, b) (a, c)
      pen (a, Left b) = Left (a, b)
      pen (a, Right c) = Right (a, c)

  parsedFiles <- fmap catMaybes $ forM files $ \fp ->
    if fileExtension fp == Just ".cent"
      then Just . pen . (,) fp <$> parseFile fp
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

  forM_ readyToFormatModules $ \((fp, textContents), newTextContents) -> do
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
