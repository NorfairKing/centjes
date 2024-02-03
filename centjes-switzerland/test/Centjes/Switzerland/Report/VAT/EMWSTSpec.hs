module Centjes.Switzerland.Report.VAT.EMWSTSpec (spec) where

import Autodocodec.Yaml
import Centjes.Compile
import Centjes.Load
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.VAT
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.TestUtils
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import Data.Maybe
import qualified Data.Text.Lazy as LT
import Data.Time
import Path.IO
import Test.Syd
import Text.XML as XML

spec :: Spec
spec = do
  dirScenarioDir "test_resources/vat" $ \fp ->
    it "Makes the same vat.xml for this scenario" $
      goldenTextFile (fp <> "vat.xml") $ do
        dir <- resolveDir' fp
        configFile <- resolveFile dir "switzerland.yaml"
        config <- do
          mConfig <- readYamlConfigFile configFile
          case mConfig of
            Nothing -> expectationFailure "Expected to find a config file."
            Just c -> pure c
        ledgerFile <- resolveFile dir $ fromMaybe "ledger.cent" $ configLedgerFile config

        (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
        ledger <- shouldValidate diag $ compileDeclarations declarations

        let pretendToday = fromGregorian 2024 01 29
        let vatInput = configureVATInput pretendToday config

        validation <- runValidationT $ runReporter $ produceVATReport vatInput ledger
        (vatReport, _) <- shouldValidate diag validation

        let pretendNow = UTCTime pretendToday 0
        xmlReport <- case produceXMLReport pretendNow vatReport of
          Nothing -> expectationFailure "Should have been able to produce a report"
          Just x -> pure x
        let xmlDoc = xmlReportDocument xmlReport

        pure $
          LT.toStrict $
            XML.renderText
              (xmlRenderSettings {rsPretty = True})
              xmlDoc
