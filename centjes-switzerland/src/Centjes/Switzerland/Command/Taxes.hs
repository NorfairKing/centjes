{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Command.Taxes (runCentjesSwitzerlandTaxes) where

import Centjes.Compile
import Centjes.Load
import Centjes.Report.Check
import Centjes.Switzerland.Assets
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.Taxes
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.Typst
import Centjes.Switzerland.Zip
import Centjes.Validation
import Conduit
import Control.Monad
import Control.Monad.Logger
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO

runCentjesSwitzerlandTaxes :: Settings -> TaxesSettings -> IO ()
runCentjesSwitzerlandTaxes Settings {..} TaxesSettings {..} = do
  mainTypContents <- requireAsset [relfile|taxes.typ|]
  withSystemTempDir "centjes-switzerland" $ \tdir -> do
    runStderrLoggingT $ do
      -- Produce the input.json structure
      (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
      ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations

      -- Check ahead of time, so we don't generate reports of invalid ledgers
      val <- liftIO $ runValidationT $ doCompleteCheck declarations
      void $ liftIO $ checkValidation diag val

      validation <- liftIO $ runValidationT $ runReporter $ produceTaxesReport taxesSettingInput ledger
      (taxesReport, files) <- liftIO $ checkValidation diag validation

      let input = taxesReportInput taxesReport

      -- Write the input to a file
      jsonInputFile <- liftIO $ do
        jif <- resolveFile tdir "input.json"
        SB.writeFile (fromAbsFile jif) (LB.toStrict (JSON.encode input))
        pure jif
      logInfoN $
        T.pack $
          unwords
            [ "Succesfully compiled information into",
              fromAbsFile jsonInputFile
            ]
      logDebugN $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty input

      -- Write the template to a file
      mainTypFile <- liftIO $ do
        mtf <- resolveFile tdir "main.typ"
        SB.writeFile (fromAbsFile mtf) $ TE.encodeUtf8 mainTypContents
        pure mtf

      -- Compile the README.pdf using typst
      liftIO $ compileTypst mainTypFile taxesSettingReadmeFile

      logInfoN $
        T.pack $
          unwords
            [ "Typst compilation succesfully created",
              fromAbsFile taxesSettingReadmeFile
            ]

      -- Create a nice zip file
      createZipFile taxesSettingZipFile $
        M.insert [relfile|README.pdf|] taxesSettingReadmeFile $
          M.insert [relfile|raw-input.json|] jsonInputFile $
            M.map (settingBaseDir </>) files

      logInfoN $
        T.pack $
          unwords
            [ "Succesfully created packet",
              fromAbsFile taxesSettingZipFile
            ]
