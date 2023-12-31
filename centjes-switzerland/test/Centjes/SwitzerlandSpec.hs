module Centjes.SwitzerlandSpec (spec) where

import Autodocodec.Yaml
import Centjes.Load
import Centjes.Switzerland
import Centjes.Switzerland.OptParse
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import Data.Maybe
import Path
import Path.IO
import qualified System.FilePath as FP
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  dirScenarioDir "test_resources/scenarios" $ \fp ->
    it "Makes the same input.json for this scenario" $
      goldenJSONValueFile (fp <> "input.json") $ do
        dir <- resolveDir' fp
        configFile <- resolveFile dir "switzerland.yaml"
        config <- do
          mConfig <- readYamlConfigFile configFile
          case mConfig of
            Nothing -> expectationFailure "Expected to find a config file."
            Just c -> pure c
        ledgerFile <- resolveFile dir $ fromMaybe "ledger.cent" $ configLedgerFile config
        (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
        (input, _) <- shouldValidateT diag $ produceInputFromDeclarations (configSetup config) declarations
        pure input

dirScenarioDir :: FilePath -> (FilePath -> TestDefM outers inner ()) -> TestDefM outers inner ()
dirScenarioDir dp func = do
  describe dp $ do
    ad <- liftIO $ resolveDir' dp
    fs <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ fst <$> listDirRel ad
    forM_ fs $ \rf -> do
      let fp = dp FP.</> fromRelDir rf
      describe (fromRelDir rf) $ func fp
