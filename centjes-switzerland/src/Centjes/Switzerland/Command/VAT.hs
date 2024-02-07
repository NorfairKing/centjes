{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Command.VAT (runCentjesSwitzerlandVAT) where

import Centjes.Command.Check
import Centjes.Compile
import Centjes.Load
import Centjes.Switzerland.Assets
import Centjes.Switzerland.Constants (development)
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.VAT
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
import System.Exit
import System.Process.Typed
import Text.Show.Pretty
import Text.XML as XML

runCentjesSwitzerlandVAT :: Settings -> VATSettings -> IO ()
runCentjesSwitzerlandVAT Settings {..} VATSettings {..} = do
  mainTypContents <- requireAsset [relfile|vat.typ|]
  xmlSchemaContents <- requireAsset [relfile|mwst-schema.xsd|]
  withSystemTempDir "centjes-switzerland" $ \tdir -> do
    runStderrLoggingT $ do
      -- Produce the input.json structure
      (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
      ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations
      -- Check ahead of time, so we don't generate reports of invalid ledgers
      val <- liftIO $ runValidationT $ doCompleteCheck declarations
      void $ liftIO $ checkValidation diag val

      validation <- liftIO $ runValidationT $ runReporter $ produceVATReport vatSettingInput ledger
      (vatReport, files) <- liftIO $ checkValidation diag validation

      let input = vatReportInput vatReport

      -- Write the xml to a file
      xmlFile <- do
        now <- liftIO getCurrentTime
        case produceXMLReport now vatReport of
          Nothing -> liftIO $ die "Failed to produce XML report. This should not happen"
          Just xmlReport -> do
            logDebugN $ T.pack $ ppShow xmlReport
            xf <- resolveFile tdir "vat.xml"
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

            -- Turned off outside of development because it does network lookups.
            when development $ do
              schemaFile <- resolveFile tdir "mwst-schema.xsd"
              liftIO $
                SB.writeFile
                  (fromAbsFile schemaFile)
                  (TE.encodeUtf8 xmlSchemaContents)
              logInfoN $
                T.pack $
                  unwords
                    [ "Validating XML output at",
                      fromAbsFile xf,
                      "against schema",
                      fromAbsFile schemaFile
                    ]
              runProcess_ $
                setWorkingDir (fromAbsDir tdir) $
                  setStdout inherit $
                    setStderr inherit $
                      proc
                        "xmllint"
                        [ "--schema",
                          fromAbsFile schemaFile,
                          fromAbsFile xf,
                          "--noout"
                        ]
            pure xf

      -- Write the input to a file
      jsonInputFile <- do
        jif <- resolveFile tdir "input.json"
        liftIO $ SB.writeFile (fromAbsFile jif) (LB.toStrict (JSON.encode input))
        logInfoN $
          T.pack $
            unwords
              [ "Succesfully compiled information into",
                fromAbsFile jif
              ]
        logDebugN $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty input
        pure jif

      -- Write the template to a file
      mainTypFile <- liftIO $ do
        mtf <- resolveFile tdir "main.typ"
        SB.writeFile (fromAbsFile mtf) (TE.encodeUtf8 mainTypContents)
        pure mtf

      liftIO $ compileTypst mainTypFile vatSettingReadmeFile

      logInfoN $
        T.pack $
          unwords
            [ "Typst compilation succesfully created",
              fromAbsFile vatSettingReadmeFile
            ]

      -- Create a nice zip file
      createZipFile vatSettingZipFile $
        M.insert [relfile|README.pdf|] vatSettingReadmeFile $
          M.insert [relfile|raw-input.json|] jsonInputFile $
            M.insert [relfile|vat.xml|] xmlFile $
              M.map (settingBaseDir </>) files

      logInfoN $
        T.pack $
          unwords
            [ "Succesfully created packet",
              fromAbsFile vatSettingZipFile
            ]
