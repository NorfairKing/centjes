{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Chart.Report.Gen where

import Centjes.AccountName.Gen ()
import Centjes.Chart.Report
import Centjes.Ledger.Gen ()
import Data.GenValidity
import Data.GenValidity.Time ()
import Data.GenValidity.Vector ()
import qualified Data.Vector as V
import Money.Account.Gen ()

instance (GenValid ann, Show ann, Ord ann) => GenValid (ChartReport ann) where
  genValid = do
    currency <- genValid
    days <- genValid
    let dayCount = V.length days
    series <- genListOf $ do
      accountName <- genValid
      -- Exactly one value per day, so the report is aligned by construction.
      values <- V.replicateM dayCount genValid
      pure (ChartSeries accountName values)
    pure (ChartReport currency days (V.fromList series))

  -- Generic shrinking would shrink the days and the series independently and
  -- break the alignment invariant, so we do not shrink.
  shrinkValid = const []
