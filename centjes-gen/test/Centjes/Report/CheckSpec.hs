{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.CheckSpec (spec) where

import Centjes.Load
import Centjes.Logging.TestUtils
import Centjes.Module.Gen ()
import Centjes.Report.Check
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec =
  describe "doCompleteCheck" $ do
    let checkDir = [reldir|test_resources/check|]
    scenarioDirRecur checkDir $ \fp -> do
      af <- liftIO $ makeAbsolute (checkDir </> fp)
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when checking this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            (ds, diag) <- runTestLoggingT $ loadModules af
            -- Try to check
            errs <- runTestLoggingT $ shouldFailToValidateT $ doCompleteCheck ds
            pure $ renderValidationErrors diag errs
    let checkValidDir = [reldir|test_resources/check-valid|]
    scenarioDirRecur checkValidDir $ \fp -> do
      af <- liftIO $ makeAbsolute (checkValidDir </> fp)
      when (fileExtension af == Just ".cent") $
        it "checks this module without errors" $ do
          (ds, diag) <- runTestLoggingT $ loadModules af
          void $ runTestLoggingT $ shouldValidateT diag $ doCompleteCheck ds
