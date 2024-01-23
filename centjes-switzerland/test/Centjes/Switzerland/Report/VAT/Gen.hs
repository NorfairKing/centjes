{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VAT.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.VAT
import Data.GenValidity
import Data.GenValidity.Time ()
import Data.Time.Calendar.Quarter
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck

instance GenValid Quarter

instance GenValid ann => GenValid (VATReport ann) where
  genValid = do
    vatReportName <- genValid
    vatReportQuarter <- genValid
    vatReportCHF <- genValid
    (vatReportForeignRevenue, vatReportDomesticRevenue, vatReportTotalRevenue) <-
      ((,) <$> genValid <*> genValid) `suchThatMap` (\(f, d) -> (,,) f d <$> Amount.add f d)
    vatReportStandardRateVAT81Percent <- genValid
    let vatReportTotalVAT = vatReportStandardRateVAT81Percent
    pure VATReport {..}
