{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.Report.VATRate.Gen ()
import Data.GenValidity
import Data.GenValidity.Time ()
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck

instance GenValid TaxesInput

instance (Show ann, Ord ann, GenValid ann) => GenValid (VATRevenue ann) where
  genValid = genValidStructurallyWithoutExtraChecking

instance (Show ann, Ord ann, GenValid ann) => GenValid (Revenue ann) where
  genValid =
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \r -> do
                        ga <- case revenueVAT r of
                          Nothing -> pure $ revenueNettoCHFAmount r
                          Just vr -> Amount.add (revenueNettoCHFAmount r) (vatRevenueCHFAmount vr)
                        pure $ r {revenueGrossCHFAmount = ga}
                    )

instance (Show ann, Ord ann, GenValid ann) => GenValid (TaxesReport ann) where
  genValid =
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \tr -> do
                        t <-
                          Amount.sum (map revenueNettoCHFAmount (taxesReportRevenues tr))
                        pure $ tr {taxesReportSelfEmploymentRevenue = t}
                    )
