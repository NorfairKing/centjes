{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.LoadSpec (spec) where

import Centjes.Load
import Centjes.Validation
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Data.List.NonEmpty (NonEmpty (..))
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec =
  describe "loadModulesOrErr" $ do
    scenarioDir "test_resources/load" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when checking this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            errOrRes <- runNoLoggingT $ runExceptT $ loadModulesOrErr af
            case errOrRes of
              Right _ -> expectationFailure "Should not have succeeded to load."
              Left (LoadError fileMap le) ->
                pure $ renderValidationErrors (diagFromFileMap' fileMap) (le :| [])
