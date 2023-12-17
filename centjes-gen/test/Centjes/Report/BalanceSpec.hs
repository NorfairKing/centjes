{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Compile
import Centjes.Format
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Report.Balance
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Path
import Path.IO
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "balanceTransaction" $ do
    it "produces valid balances" $
      producesValid $
        balanceTransaction @()

  let usdSymbol = CurrencySymbol "USD"
  let usdDeclaration =
        DeclarationCurrency $
          noLoc $
            CurrencyDeclaration
              { currencyDeclarationSymbol = noLoc usdSymbol,
                currencyDeclarationQuantisationFactor = noLoc "0.01"
              }
  let eurSymbol = CurrencySymbol "EUR"
  let eurDeclaration =
        DeclarationCurrency $
          noLoc $
            CurrencyDeclaration
              { currencyDeclarationSymbol = noLoc eurSymbol,
                currencyDeclarationQuantisationFactor = noLoc "0.01"
              }

  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      producesValid2
        (produceBalanceReport @())

    scenarioDir "test_resources/balance/balanced" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $
        it "balances this module" $ do
          -- Load the module
          (ds, diag) <- runNoLoggingT $ loadModules af
          -- Compile to a ledger
          ledger <- shouldValidate diag $ compileDeclarations ds

          br <- shouldValidate diag $ produceBalanceReport Nothing ledger
          shouldBeValid br

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

            errs <- shouldFailToValidate $ produceBalanceReport Nothing ledger
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

            errs <- shouldFailToValidate $ produceBalanceReport (Just (CurrencySymbol "CHF")) ledger
            pure $ renderValidationErrors diag errs
