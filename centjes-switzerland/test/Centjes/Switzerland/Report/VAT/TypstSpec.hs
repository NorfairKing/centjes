{-# LANGUAGE RecordWildCards #-}

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
import Data.Time.Calendar.Quarter
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland
import System.Environment
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = sequential $
  dirScenarioDir "test_resources/vat" $ \fp ->
    it "Makes the same input.json for this scenario" $
      goldenJSONFile (fp <> "vat-input.json") $ do
        dir <- resolveDir' fp
        withCurrentDir dir $ withArgs [] $ do
          (Settings {..}, VATSettings {..}) <- runParser version "centjes-switzerland vat-tests" $ (,) <$> settingsParser <*> settingsParser

          let ledgerFile = settingBaseDir </> settingLedgerFile

          (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
          ledger <- shouldValidate diag $ compileDeclarations declarations

          let pretendToday = fromGregorian 2024 02 03
          let vatInput = vatSettingInput {vatInputQuarter = YearQuarter 2024 Q1}

          validation <- runValidationT $ runReporter $ produceVATReport vatInput ledger
          (vatReport, _) <- shouldValidate diag validation
          let input = vatReportInput vatReport

          pure $ toJSON input
