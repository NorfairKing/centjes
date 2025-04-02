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
import qualified Data.Text.Lazy as LT
import Data.Time
import Path
import Path.IO
import Paths_centjes_switzerland
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed
import Text.Show.Pretty
import Text.XML as XML

runCentjesSwitzerlandTaxes :: Settings -> TaxesSettings -> IO ()
runCentjesSwitzerlandTaxes Settings {..} TaxesSettings {..} = do
  dataDir <- Paths_centjes_switzerland.getDataDir >>= resolveDir'
  assetsDir <- resolveDir dataDir "assets"
  typstTemplateFile <- resolveFile assetsDir "taxes.typ"
  schemaDir <- resolveDir assetsDir "schemas"
  catalogFile <- resolveFile schemaDir "catalog.xml"
  schemaFile <- resolveFile schemaDir "eCH-0119-4-0-0.xsd"

  withSystemTempDir "centjes-switzerland" $ \tdir -> do
    runStderrLoggingT $ do
      -- Produce the input.json structure
      (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
      ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations

      -- Check ahead of time, so we don't generate reports of invalid ledgers
      val <- runValidationT $ doCompleteCheck declarations
      void $ liftIO $ checkValidation diag val

      validation <- liftIO $ runValidationT $ runReporter $ produceTaxesReport taxesSettingInput ledger
      (taxesReport, files) <- liftIO $ checkValidation diag validation

      -- Write the xml to a file
      xmlFile <- do
        now <- liftIO getCurrentTime
        case produceXMLReport now taxesReport of
          Nothing -> liftIO $ die "Failed to produce XML report. This should not happen"
          Just xmlReport -> do
            logDebugN $ T.pack $ ppShow xmlReport
            xf <- resolveFile tdir "taxes.xml"
            let xmlDoc = xmlReportDocument xmlReport

            liftIO $
              XML.writeFile
                xmlRenderSettings
                (fromAbsFile xf)
                xmlDoc
            logInfoN $
              T.pack $
                unwords
                  [ "Wrote XML version to",
                    fromAbsFile xf
                  ]
            logDebugN $
              LT.toStrict $
                XML.renderText
                  (xmlRenderSettings {rsPretty = True})
                  xmlDoc

            logInfoN $
              T.pack $
                unwords
                  [ "Validating XML output at",
                    fromAbsFile xf,
                    "against schema",
                    fromAbsFile schemaFile
                  ]
            environment <- liftIO getEnvironment
            let newEnvironment = ("SGML_CATALOG_FILES", fromAbsFile catalogFile) : environment
            runProcess_ $
              setWorkingDir (fromAbsDir tdir) $
                setEnv newEnvironment $
                  setStdout inherit $
                    setStderr inherit $
                      proc
                        "xmllint"
                        [ "--debugent",
                          "--noout",
                          "--schema",
                          fromAbsFile schemaFile,
                          fromAbsFile xf,
                          "--catalogs"
                        ]
            pure xf

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
        copyFile typstTemplateFile mtf
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
            M.insert [relfile|taxes.xml|] xmlFile $
              M.map (settingBaseDir </>) files

      logInfoN $
        T.pack $
          unwords
            [ "Succesfully created packet",
              fromAbsFile taxesSettingZipFile
            ]
