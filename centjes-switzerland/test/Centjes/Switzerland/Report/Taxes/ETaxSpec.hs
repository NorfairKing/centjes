{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes.ETaxSpec (spec) where

import Centjes.Compile
import Centjes.Load
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.Taxes
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.TestUtils
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import qualified Data.Text.Lazy as LT
import Data.Time
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland (version)
import System.Environment
import Test.Syd
import Text.XML as XML

spec :: Spec
spec = sequential $
  dirScenarioDir "test_resources" $ \fp ->
    it "Makes the same taxes.xml for this scenario" $
      goldenTextFile (fp <> "taxes.xml") $ do
        dir <- resolveDir' fp
        withCurrentDir dir $ withArgs [] $ do
          configFile <- resolveFile dir "switzerland.yaml"
          (Settings {..}, TaxesSettings {..}) <-
            runParser version "centjes-switzerland taxes-tests" $
              withYamlConfig (pure (Just configFile)) $
                (,) <$> settingsParser <*> settingsParser

          let ledgerFile = settingBaseDir </> settingLedgerFile
          (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
          ledger <- shouldValidate diag $ compileDeclarations declarations

          let pretendToday = fromGregorian 2024 02 07
          let taxesInput = taxesSettingInput {taxesInputYear = 2024}

          validation <- runValidationT $ runReporter $ produceTaxesReport taxesInput ledger
          (taxesReport, _) <- shouldValidate diag validation

          let pretendNow = UTCTime pretendToday 0
          xmlReport <- case produceXMLReport pretendNow taxesReport of
            Nothing -> expectationFailure "Should have been able to produce a report"
            Just x -> pure x
          let xmlDoc = xmlReportDocument xmlReport

          pure $
            LT.toStrict $
              XML.renderText
                (xmlRenderSettings {rsPretty = True})
                xmlDoc
