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
import qualified Data.Vector as V
import qualified Money.MultiAccount as MultiAccount
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      forAllValid $ \f ->
        forAllValid $ \mCurrencySymbolTo ->
          forAllValid $ \showVirtual ->
            forAllValid $ \ledger ->
              shouldBeValid $ produceRegister @() f mCurrencySymbolTo showVirtual ledger

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

            br <-
              shouldValidate diag $
                produceRegister
                  FilterAny
                  Nothing
                  False
                  ledger
            shouldBeValid br
            pure $ renderChunksText With24BitColours $ renderRegisterTable br

        it "produces reports that balance to empty amounts" $ do
          -- Load the module
          (ds, diag) <- runNoLoggingT $ loadModules af
          -- Compile to a ledger
          ledger <- shouldValidate diag $ compileDeclarations ds

          Register transactions <-
            shouldValidate diag $
              produceRegister
                FilterAny
                Nothing
                False
                ledger

          if null transactions
            then pure ()
            else do
              let (_, _, postings) = V.last transactions
              if null postings
                then pure ()
                else do
                  let (_, acc) = V.last postings
                  acc `shouldBe` MultiAccount.zero

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

            br <-
              shouldValidate diag $
                produceRegister
                  FilterAny
                  (Just (CurrencySymbol "CHF"))
                  False
                  ledger
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

            errs <-
              shouldFailToValidate $
                produceRegister
                  FilterAny
                  Nothing
                  False
                  ledger
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

            errs <-
              shouldFailToValidate $
                produceRegister
                  FilterAny
                  (Just (CurrencySymbol "CHF"))
                  False
                  ledger
            pure $ renderValidationErrors diag errs
