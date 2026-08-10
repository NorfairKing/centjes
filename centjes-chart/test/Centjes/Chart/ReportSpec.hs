{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Chart.ReportSpec (spec) where

import Centjes.AccountType (AccountType (..))
import Centjes.Chart.Report
import Centjes.Chart.Report.Gen ()
import Centjes.Compile
import Centjes.CurrencySymbol (CurrencySymbol (..))
import Centjes.Filter (Filter (FilterAny))
import Centjes.Ledger (Currency)
import Centjes.Load
import Centjes.Report.EvaluatedLedger
import Centjes.Validation (Validation (..), renderValidationErrors)
import Centjes.Validation.TestUtils
import Control.Monad (forM_, when)
import Control.Monad.Logger
import Data.GenValidity.Time ()
import qualified Data.Map.Strict as M
import Data.Time (Day)
import qualified Data.Vector as V
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "ChartReport" $
    genValidSpec @(ChartReport ())

  describe "Validity (ChartReport ())" $
    it "rejects a report whose series does not have one value per day" $
      forAllValid $ \(currency :: Currency ()) ->
        forAllValid $ \(day :: Day) ->
          forAllValid $ \accountName ->
            forAllValid $ \(values :: [Money.Account]) ->
              when (length values /= 1) $
                shouldBeInvalid $
                  ChartReport
                    currency
                    (V.singleton day)
                    (V.singleton (ChartSeries accountName (V.fromList values)))

  describe "chartReportIsNonNegative" $ do
    it "holds when every value is non-negative" $
      forAllValid $ \(currency :: Currency ()) ->
        forAllValid $ \day ->
          forAllValid $ \accountName ->
            forAllValid $ \(value :: Money.Account) ->
              when (value >= Account.zero) $
                chartReportIsNonNegative
                  (ChartReport currency (V.singleton day) (V.singleton (ChartSeries accountName (V.singleton value))))
                  `shouldBe` True
    it "fails when some value is negative" $
      forAllValid $ \(currency :: Currency ()) ->
        forAllValid $ \day ->
          forAllValid $ \accountName ->
            forAllValid $ \(value :: Money.Account) ->
              when (value < Account.zero) $
                chartReportIsNonNegative
                  (ChartReport currency (V.singleton day) (V.singleton (ChartSeries accountName (V.singleton value))))
                  `shouldBe` False

  describe "sampleStepFunction" $ do
    it "produces one value per day" $
      forAllValid $ \(initial :: Int, knownList :: [(Day, Int)], days :: [Day]) ->
        V.length (sampleStepFunction initial (V.fromList days) (M.fromList knownList))
          `shouldBe` length days
    it "returns the default for every day when nothing is known" $
      forAllValid $ \(initial :: Int, days :: [Day]) ->
        sampleStepFunction initial (V.fromList days) M.empty
          `shouldBe` V.replicate (length days) initial
    it "passes through each known value on its own day" $
      forAllValid $ \(initial :: Int, knownList :: [(Day, Int)]) -> do
        let known = M.fromList knownList
        forM_ (M.toList known) $ \(day, value) ->
          sampleStepFunction initial (V.singleton day) known
            `shouldBe` V.singleton value

  describe "averageHeld" $ do
    it "is zero for an empty series" $
      averageHeld V.empty `shouldBe` 0
    it "is the value itself for a series that never moves" $
      forAllValid $ \(value :: Rational) ->
        forAllValid $ \(days :: [Day]) ->
          when (not (null days)) $
            averageHeld (V.replicate (length days) value)
              `shouldBe` abs value

  describe "turnover" $ do
    it "is zero for a series that never moves" $
      forAllValid $ \(value :: Rational) ->
        forAllValid $ \(days :: [Day]) ->
          turnover (V.replicate (length days) value)
            `shouldBe` 0

    it "is the flow over the average held" $
      -- 10 down to 0 in steps of 2: 10 flowed out of an account holding 5 on
      -- average, so it turned over twice.
      turnover (V.fromList [10, 8, 6, 4, 2, 0])
        `shouldBe` 2

    it "counts money coming back in as more flow, not less" $
      turnover (V.fromList [10, 0, 10])
        `shouldBe` 3

    it "does not depend on the scale of the account" $
      forAllValid $ \(values :: [Rational]) ->
        forAllValid $ \(scale :: Rational) ->
          when (scale > 0) $
            turnover (V.fromList (map (* scale) values))
              `shouldBe` turnover (V.fromList values)

  describe "produceChartReport" $
    scenarioDir "test_resources/chart/valid" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        it "produces the same chart report" $
          goldenTextFile (fromAbsFile reportFile) $ do
            (ds, diag) <- runNoLoggingT $ loadModules af
            ledger <- shouldValidate diag $ compileDeclarations ds
            evaluatedLedger <- shouldValidate diag $ produceEvaluatedLedger ledger
            report <-
              shouldValidate diag $
                produceChartReport [AccountTypeAssets] FilterAny (CurrencySymbol "USD") evaluatedLedger
            pure $ renderChartReportText report

  describe "produceChartReport (errors)" $ do
    do
      af <- liftIO $ resolveFile' "test_resources/chart/error/unknown-currency.cent"
      errFile <- liftIO $ replaceExtension ".err" af
      it "fails when the chart currency is not declared" $
        goldenTextFile (fromAbsFile errFile) $ do
          (ds, diag) <- runNoLoggingT $ loadModules af
          ledger <- shouldValidate diag $ compileDeclarations ds
          evaluatedLedger <- shouldValidate diag $ produceEvaluatedLedger ledger
          case produceChartReport [AccountTypeAssets] FilterAny (CurrencySymbol "XYZ") evaluatedLedger of
            Success _ -> expectationFailure "Expected a chart error but produced a report."
            Failure errs -> pure $ renderValidationErrors diag errs
    do
      af <- liftIO $ resolveFile' "test_resources/chart/error/no-matching-accounts.cent"
      errFile <- liftIO $ replaceExtension ".err" af
      it "fails when no account passes the filter" $
        goldenTextFile (fromAbsFile errFile) $ do
          (ds, diag) <- runNoLoggingT $ loadModules af
          ledger <- shouldValidate diag $ compileDeclarations ds
          evaluatedLedger <- shouldValidate diag $ produceEvaluatedLedger ledger
          case produceChartReport [AccountTypeAssets] FilterAny (CurrencySymbol "USD") evaluatedLedger of
            Success _ -> expectationFailure "Expected a chart error but produced a report."
            Failure errs -> pure $ renderValidationErrors diag errs
    do
      af <- liftIO $ resolveFile' "test_resources/chart/error/missing-price.cent"
      errFile <- liftIO $ replaceExtension ".err" af
      it "fails when a held currency cannot be converted" $
        goldenTextFile (fromAbsFile errFile) $ do
          (ds, diag) <- runNoLoggingT $ loadModules af
          ledger <- shouldValidate diag $ compileDeclarations ds
          evaluatedLedger <- shouldValidate diag $ produceEvaluatedLedger ledger
          case produceChartReport [AccountTypeAssets] FilterAny (CurrencySymbol "USD") evaluatedLedger of
            Success _ -> expectationFailure "Expected a chart error but produced a report."
            Failure errs -> pure $ renderValidationErrors diag errs
