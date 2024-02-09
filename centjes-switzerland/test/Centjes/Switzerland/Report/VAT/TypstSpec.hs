module Centjes.Switzerland.Report.VAT.TypstSpec (spec) where

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
import Data.Aeson
import Data.Maybe
import Data.Time
import Path.IO
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  dirScenarioDir "test_resources/vat" $ \fp ->
    it "Makes the same input.json for this scenario" $
      goldenJSONFile (fp <> "vat-input.json") $ do
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

        let pretendToday = fromGregorian 2024 02 03
        let vatInput = configureVATInput pretendToday config
        validation <- runValidationT $ runReporter $ produceVATReport vatInput ledger
        (vatReport, _) <- shouldValidate diag validation
        let input = vatReportInput vatReport

        pure $ toJSON input
