{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.RegisterSpec (spec) where

import Centjes.Command.Register (renderRegisterTable)
import Centjes.Compile
import Centjes.CurrencySymbol (CurrencySymbol (..))
import Centjes.Filter (Filter (..))
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Report.Register
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      producesValid3
        (produceRegister @())

    scenarioDir "test_resources/register/valid/as-is" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        it "renders a register for this module the same way" $ do
          goldenTextFile (fromAbsFile reportFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            br <- shouldValidate diag $ produceRegister FilterAny Nothing ledger
            shouldBeValid br
            pure $ renderChunksText With24BitColours $ renderRegisterTable br

    scenarioDir "test_resources/register/valid/to-chf" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        it "renders a register for this module the same way" $ do
          goldenTextFile (fromAbsFile reportFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            br <- shouldValidate diag $ produceRegister FilterAny (Just (CurrencySymbol "CHF")) ledger
            shouldBeValid br
            pure $ renderChunksText With24BitColours $ renderRegisterTable br

    scenarioDir "test_resources/register/error/as-is" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when trying to balance this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            errs <- shouldFailToValidate $ produceRegister FilterAny Nothing ledger
            pure $ renderValidationErrors diag errs

    scenarioDir "test_resources/register/error/to-chf" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when trying to balance this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            errs <- shouldFailToValidate $ produceRegister FilterAny (Just (CurrencySymbol "CHF")) ledger
            pure $ renderValidationErrors diag errs
