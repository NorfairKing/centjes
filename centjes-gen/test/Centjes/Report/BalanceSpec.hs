{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Command.Balance (renderBalanceReportTable)
import Centjes.Compile
import Centjes.Filter (Filter (..))
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Module
import Centjes.OptParse
import Centjes.Report.Balance
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
  describe "balanceTransaction" $ do
    it "produces valid balances" $
      producesValid $
        balanceTransaction @()

  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      producesValid3
        (produceBalanceReport @())

    scenarioDir "test_resources/balance/balanced/as-is" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        it "balances this module the same way" $ do
          goldenTextFile (fromAbsFile reportFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            br <- shouldValidate diag $ produceBalanceReport FilterAny Nothing ledger
            shouldBeValid br
            pure $ renderChunksText With24BitColours $ renderBalanceReportTable ShowEmpty br

    scenarioDir "test_resources/balance/balanced/to-chf" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        it "balances this module the same way" $ do
          goldenTextFile (fromAbsFile reportFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            br <- shouldValidate diag $ produceBalanceReport FilterAny (Just (CurrencySymbol "CHF")) ledger
            shouldBeValid br
            pure $ renderChunksText With24BitColours $ renderBalanceReportTable ShowEmpty br

    scenarioDir "test_resources/balance/error/as-is" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when trying to balance this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            errs <- shouldFailToValidate $ produceBalanceReport FilterAny Nothing ledger
            pure $ renderValidationErrors diag errs

    scenarioDir "test_resources/balance/error/to-chf" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when trying to balance this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            errs <- shouldFailToValidate $ produceBalanceReport FilterAny (Just (CurrencySymbol "CHF")) ledger
            pure $ renderValidationErrors diag errs
