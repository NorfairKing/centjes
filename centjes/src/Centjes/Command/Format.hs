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
formatDir = walkDir $ \_ _ files -> do
  mapM_ formatFile files
  pure $ WalkExclude [] -- Exclude hidden dirs and files?

formatFile :: Path Abs File -> LoggingT IO ()
formatFile fp =
  case fileExtension fp of
    Just ".cent" -> formatSingleFile fp
    _ -> pure ()

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
      let newTextContents = formatModule m
      case parseModule "idempotence-test" newTextContents of
        Left err ->
          liftIO $
            die $
              unlines
                [ "Formatted file could not be parsed, this indicates a bug:",
                  err,
                  T.unpack newTextContents
                ]
        Right m' ->
          when (newTextContents /= formatModule m') $
            liftIO $
              die $
                unlines
                  [ "Formatting was not idempotent.",
                    "Before:",
                    show m,
                    "After:",
                    show m'
                  ]
      when (newTextContents /= textContents) $
        liftIO $
          SB.writeFile (fromAbsFile fp) $
            TE.encodeUtf8 newTextContents
      logInfoN $ T.pack $ unwords ["Formatted", fromAbsFile fp]

parseFile :: Path Abs File -> LoggingT IO (Either String (Text, Module))
parseFile fp = do
  contents <- liftIO $ SB.readFile (fromAbsFile fp)
  case TE.decodeUtf8' contents of
    Left err -> pure (Left (show err))
    Right textContents ->
      pure $ case parseModule (fromAbsFile fp) textContents of
        Left err -> Left err
        Right m -> Right (textContents, m)
