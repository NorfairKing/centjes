{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Chart.RenderSpec (spec) where

import Centjes.Chart.Render
import Centjes.Chart.Report
import Centjes.Chart.Report.Gen ()
import Centjes.Ledger (currencyQuantisationFactor)
import Centjes.Location
import Control.Monad (zipWithM_)
import Data.List (transpose)
import qualified Data.Vector as V
import qualified Money.Account as Account
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "stackBands" $
    it "stacks the series into bands that partition each day's total from zero" $
      forAllValid $ \(report :: ChartReport ()) ->
        let Located _ quantisationFactor = currencyQuantisationFactor (chartReportCurrency report)
            toDouble = Account.toDouble quantisationFactor
            days = V.toList (chartReportDays report)
            series = V.toList (chartReportSeries report)
            bands = stackBands toDouble days series
            -- The (lower, upper) of each band, regrouped per day.
            bandsPerDay = transpose (map (map snd . snd) bands)
            -- The series values, regrouped per day.
            valuesPerDay = transpose (map (map toDouble . V.toList . chartSeriesValues) series)
         in zipWithM_
              ( \values dayBands ->
                  let uppers = scanl1 (+) values
                      lowers = 0 : init uppers
                   in dayBands `shouldBe` zip lowers uppers
              )
              valuesPerDay
              bandsPerDay

  describe "lineSeries" $
    it "produces one point per day for each series" $
      forAllValid $ \(report :: ChartReport ()) ->
        let Located _ quantisationFactor = currencyQuantisationFactor (chartReportCurrency report)
            toDouble = Account.toDouble quantisationFactor
            days = V.toList (chartReportDays report)
            series = V.toList (chartReportSeries report)
         in map (length . snd) (lineSeries toDouble days series)
              `shouldBe` map (const (length days)) series
