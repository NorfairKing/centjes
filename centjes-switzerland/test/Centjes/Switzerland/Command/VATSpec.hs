module Centjes.Switzerland.Command.VATSpec (spec) where

import Autodocodec.Yaml
import Centjes.Load
import Centjes.Switzerland.Command.VAT
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.TestUtils
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import Data.Aeson
import Data.Maybe
import Path.IO
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  dirScenarioDir "test_resources/vat" $ \fp ->
    it "Makes the same input.json for this scenario" $
      goldenJSONFile (fp <> "input.json") $ do
        dir <- resolveDir' fp
        configFile <- resolveFile dir "switzerland.yaml"
        config <- do
          mConfig <- readYamlConfigFile configFile
          case mConfig of
            Nothing -> expectationFailure "Expected to find a config file."
            Just c -> pure c
        ledgerFile <- resolveFile dir $ fromMaybe "ledger.cent" $ configLedgerFile config
        (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
        (input, _) <- shouldValidateT diag $ produceVATInputFromDeclarations (configSetup config) declarations
        pure $ toJSON input
