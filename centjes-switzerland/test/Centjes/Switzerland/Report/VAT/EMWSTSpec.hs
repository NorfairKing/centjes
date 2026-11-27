{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.VAT.EMWSTSpec (spec) where

import Centjes.Compile
import Centjes.Load
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.VAT
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.TestUtils
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.Calendar.Quarter
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland (version)
import System.Environment
import Test.Syd
import Text.XML as XML

spec :: Spec
spec = sequential $
  dirScenarioDir [reldir|test_resources|] $ \rd ->
    it "Makes the same vat.xml for this scenario" $
      goldenTextFile (fromRelFile (rd </> [relfile|vat.xml|])) $ do
        dir <- makeAbsolute rd
        withCurrentDir dir $ withArgs [] $ do
          configFile <- resolveFile dir "switzerland.yaml"
          (Settings {..}, VATSettings {..}) <-
            runParser version "centjes-switzerland vat-tests" $
              withYamlConfig (pure (Just configFile)) $
                (,) <$> settingsParser <*> settingsParser

          let ledgerFile = settingsBaseDir </> settingsLedgerFile

          (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
          ledger <- shouldValidate diag $ compileDeclarations declarations

          let pretendToday = fromGregorian 2024 01 29
          let vatInput = vatSettingsInput {vatInputQuarter = YearQuarter 2024 Q1}

          let validation = runReporter $ produceVATReport vatInput ledger
          (vatReport, _) <- shouldValidate diag validation

          let pretendNow = UTCTime pretendToday 0
          xmlReport <- case produceXMLReport pretendNow vatReport of
            Nothing -> expectationFailure "Should have been able to produce a report"
            Just x -> pure x
          let xmlDoc = xmlReportDocument xmlReport

          pure $
            LT.toStrict $
              XML.renderText
                xmlRenderSettings
                xmlDoc
