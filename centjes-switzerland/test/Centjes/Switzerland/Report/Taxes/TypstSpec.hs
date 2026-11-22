{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes.TypstSpec (spec) where

import Centjes.Compile
import Centjes.Load
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.Taxes
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.TestUtils
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import Data.Aeson
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland (version)
import System.Environment
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = sequential $
  dirScenarioDir "test_resources" $ \fp ->
    it "Makes the same taxes-input.json for this scenario" $
      goldenJSONFile (fp <> "taxes-input.json") $ do
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

          let taxesInput = taxesSettingInput {taxesInputYear = 2024}

          let validation = runReporter $ produceTaxesReport taxesInput ledger
          (taxesReport, _) <- shouldValidate diag validation

          pure $ toJSON $ taxesReportInput taxesReport
