{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.LoadSpec (spec) where

import Centjes.Load
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
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
            validation <- runNoLoggingT $ unValidationT $ loadModulesOrErr af
            errs <- shouldFailToValidate validation
            pure $ renderValidationErrors mempty errs
