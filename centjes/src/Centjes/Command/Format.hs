{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Format (runCentjesFormat) where

import Centjes.Compile
import Centjes.Format
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.OptParse
import Centjes.Parse
import Centjes.Validation
import Control.Arrow (second)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Either
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Money.QuantisationFactor as QuantisationFactor
import Numeric.DecimalLiteral as DecimalLiteral
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
  (ds, fileMap) <- loadModules' l
  let diag = diagFromFileMap fileMap
  currencies <- liftIO $ checkValidation diag $ compileCurrencyDeclarations ds

  let base = parent l
  forM_ (M.toList fileMap) $ \(fp, (textContents, m)) ->
    formatSingleFileWith base fp textContents (fillInDigits currencies m)

formatDir :: Map CurrencySymbol (GenLocated ann QuantisationFactor) -> Path Abs Dir -> LoggingT IO ()
formatDir currencies base = do
  files <- snd <$> listDirRecurRel base

  let pen :: (a, Either b c) -> Either (a, b) (a, c)
      pen (a, Left b) = Left (a, b)
      pen (a, Right c) = Right (a, c)

  parsedFiles <- fmap catMaybes $ forConcurrently files $ \fp ->
    if fileExtension fp == Just ".cent"
      then Just . pen . (,) fp . fmap (second (fillInDigits currencies)) <$> parseFile base fp
      else pure Nothing

  parsedModules <- case partitionEithers parsedFiles of
    ([], ms) -> pure ms
    (errs, _) -> do
      logErrorN $
        T.pack $
          unlines $
            flip map errs $ \(fp, err) ->
              concat
                [ unwords ["Failed to parse", fromAbsFile (base </> fp)],
                  "\n",
                  err
                ]
      liftIO $ die "Could not parse all files. Not continuing to formatting them."

  let checkedModules = map (\(fp, (textContents, m)) -> pen ((fp, textContents), idempotenceTest base fp m)) parsedModules

  readyToFormatModules <- case partitionEithers checkedModules of
    ([], ms) -> pure ms
    (errs, _) -> do
      logErrorN $
        T.pack $
          unlines $
            flip map errs $ \((fp, _), err) ->
              concat
                [ unwords ["Idempotence test failed for", fromAbsFile (base </> fp)],
                  "\n",
                  err
                ]
      liftIO $ die "At least one file failed the idempotence test. Not continuing to formatting them."

  forConcurrently_ readyToFormatModules $ \((fp, textContents), newTextContents) -> do
    formatFile base fp textContents newTextContents

-- Format a single file on its own.
-- This ignores any declarations.
formatSingleFile :: Path Abs File -> LoggingT IO ()
formatSingleFile fp = do
  let base = parent fp
  let rf = filename fp
  errOrModule <- parseFile base rf
  case errOrModule of
    Left err ->
      logWarnN $
        T.pack $
          unlines
            [ "Could not format file because it does not look like Utf-8: ",
              show fp,
              show err
            ]
    Right (textContents, m) -> formatSingleFileWith base rf textContents m

formatSingleFileWith :: Path Abs Dir -> Path Rel File -> Text -> LModule -> LoggingT IO ()
formatSingleFileWith base rf textContents m = do
  newTextContents <- case idempotenceTest base rf m of
    Left err -> liftIO $ die err
    Right ft -> pure ft
  formatFile base rf textContents newTextContents

parseFile :: Path Abs Dir -> Path Rel File -> LoggingT IO (Either String (Text, LModule))
parseFile base fp = do
  contents <- liftIO $ SB.readFile (fromAbsFile (base </> fp))
  case TE.decodeUtf8' contents of
    Left err -> pure (Left (show err))
    Right textContents ->
      pure $ case parseModule base fp textContents of
        Left err -> Left err
        Right m -> Right (textContents, m)

formatFile :: Path Abs Dir -> Path Rel File -> Text -> Text -> LoggingT IO ()
formatFile base rf textContents newTextContents =
  if newTextContents == textContents
    then logDebugN $ T.pack $ unwords ["Did not format because nothing changed:", fromAbsFile (base </> rf)]
    else do
      liftIO $ SB.writeFile (fromAbsFile (base </> rf)) $ TE.encodeUtf8 newTextContents
      logInfoN $ T.pack $ unwords ["Formatted", fromAbsFile (base </> rf)]

idempotenceTest :: Path Abs Dir -> Path Rel File -> LModule -> Either String Text
idempotenceTest base rf m = do
  let newTextContents = formatModule m
  case parseModule base [relfile|idempotence-test|] newTextContents of
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
                fromAbsFile (base </> rf),
                "Before:",
                show m,
                "After:",
                show m'
              ]

fillInDigits :: Map CurrencySymbol (GenLocated ann QuantisationFactor) -> LModule -> LModule
fillInDigits currencies m = m {moduleDeclarations = map goDeclaration (moduleDeclarations m)}
  where
    goDeclaration :: Declaration ann -> Declaration ann
    goDeclaration = \case
      DeclarationTransaction t -> DeclarationTransaction $ goTransaction <$> t
      d -> d
    goTransaction :: Transaction ann -> Transaction ann
    goTransaction t = t {transactionPostings = map (fmap goPosting) (transactionPostings t)}
    goPosting :: Posting ann -> Posting ann
    goPosting p =
      let Located _ symbol = postingCurrencySymbol p
       in case M.lookup symbol currencies of
            Nothing -> p
            Just (Located _ qf) ->
              let Located l account = postingAccount p
               in p {postingAccount = Located l $ DecimalLiteral.setMinimumDigits (QuantisationFactor.digits qf) account}
