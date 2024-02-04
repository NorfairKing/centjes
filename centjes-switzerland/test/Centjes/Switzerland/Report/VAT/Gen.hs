{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VAT.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.VAT
import Data.GenValidity
import Data.GenValidity.Time ()
import Data.Time.Calendar.Quarter
import qualified Money.Account as Account
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck

-- TODO upstream this to genvalidity-time
instance GenValid Quarter

instance GenValid VATInput

instance GenValid VATRate

instance (Show ann, Ord ann, GenValid ann) => GenValid (DomesticRevenue ann) where
  genValid = do
    dr <- genValidStructurally
    -- If this number is too small, that's a VERY good problem.
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    vatAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      dr
        { domesticRevenueCHFAmount = chfAmount,
          domesticRevenueVATCHFAmount = vatAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (ForeignRevenue ann) where
  genValid = do
    dr <- genValidStructurally
    -- If this number is too small, that's a VERY good problem.
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $ dr {foreignRevenueCHFAmount = chfAmount}

instance (Show ann, Ord ann, GenValid ann) => GenValid (DeductibleExpense ann) where
  genValid = do
    dr <- genValidStructurally
    -- If this number is too small, that's a VERY good problem.
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    vatAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      dr
        { deductibleExpenseCHFAmount = chfAmount,
          deductibleExpenseVATCHFAmount = vatAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (VATReport ann) where
  genValid = do
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \vr -> do
                        totalDomesticRevenues <-
                          Amount.sum
                            (map domesticRevenueCHFAmount (vatReportDomesticRevenues vr))
                        pure $ vr {vatReportTotalDomesticRevenue = totalDomesticRevenues}
                    )
      `suchThatMap` ( \vr -> do
                        totalExportsRevenues <-
                          Amount.sum
                            (map foreignRevenueCHFAmount (vatReportExportsRevenues vr))
                        pure $ vr {vatReportTotalExportsRevenue = totalExportsRevenues}
                    )
      `suchThatMap` ( \vr -> do
                        totalForeignRevenues <-
                          Amount.sum
                            (map foreignRevenueCHFAmount (vatReportForeignRevenues vr))
                        pure $ vr {vatReportTotalForeignRevenue = totalForeignRevenues}
                    )
      `suchThatMap` ( \vr -> do
                        totalForeignDeductions <-
                          Amount.sum
                            [ vatReportTotalForeignRevenue vr,
                              vatReportTotalExportsRevenue vr
                            ]
                        pure $ vr {vatReportTotalForeignDeductions = totalForeignDeductions}
                    )
      `suchThatMap` ( \vr -> do
                        totalRevenue <-
                          Amount.sum
                            [ vatReportTotalForeignRevenue vr,
                              vatReportTotalExportsRevenue vr,
                              vatReportTotalDomesticRevenue vr
                            ]
                        pure $ vr {vatReportTotalRevenue = totalRevenue}
                    )
      `suchThatMap` ( \vr -> do
                        standardRate2023VATRevenue <-
                          Amount.sum
                            ( map
                                domesticRevenueVATCHFAmount
                                (filter ((== VATRate2023Standard) . domesticRevenueVATRate) (vatReportDomesticRevenues vr))
                            )
                        pure $ vr {vatReport2023StandardRateVATRevenue = standardRate2023VATRevenue}
                    )
      `suchThatMap` ( \vr -> do
                        standardRate2024VATRevenue <-
                          Amount.sum
                            ( map
                                domesticRevenueVATCHFAmount
                                (filter ((== VATRate2024Standard) . domesticRevenueVATRate) (vatReportDomesticRevenues vr))
                            )
                        pure $ vr {vatReport2024StandardRateVATRevenue = standardRate2024VATRevenue}
                    )
      `suchThatMap` ( \vr -> do
                        totalVATRevenue <-
                          Amount.sum
                            [ vatReport2023StandardRateVATRevenue vr,
                              vatReport2024StandardRateVATRevenue vr
                            ]
                        pure $ vr {vatReportTotalVATRevenue = totalVATRevenue}
                    )
      `suchThatMap` ( \vr -> do
                        totalVATDeductions <-
                          Amount.sum
                            [vatReportPaidVAT vr]
                        pure $ vr {vatReportTotalVATDeductions = totalVATDeductions}
                    )
      `suchThatMap` ( \vr -> do
                        payable <-
                          Account.subtract
                            (Account.fromAmount (vatReportTotalVATRevenue vr))
                            (Account.fromAmount (vatReportPaidVAT vr))
                        pure $ vr {vatReportPayable = payable}
                    )
