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

instance GenValid VATRate

instance (Show ann, Ord ann, GenValid ann) => GenValid (DomesticRevenue ann)

instance (Show ann, Ord ann, GenValid ann) => GenValid (VATReport ann) where
  genValid =
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \vr -> do
                        totalDomesticRevenues <- Amount.sum (map domesticRevenueCHFAmount (vatReportDomesticRevenues vr))
                        pure $ vr {vatReportTotalDomesticRevenue = totalDomesticRevenues}
                    )
      `suchThatMap` ( \vr -> do
                        totalRevenue <- Amount.add (vatReportForeignRevenue vr) (vatReportTotalDomesticRevenue vr)
                        pure $ vr {vatReportTotalRevenue = totalRevenue}
                    )
      `suchThatMap` ( \vr -> do
                        standardRate2023VATRevenue <- Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2023Standard) . domesticRevenueVATRate) (vatReportDomesticRevenues vr)))
                        pure $ vr {vatReport2023StandardRateVATRevenue = standardRate2023VATRevenue}
                    )
      `suchThatMap` ( \vr -> do
                        standardRate2024VATRevenue <- Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2024Standard) . domesticRevenueVATRate) (vatReportDomesticRevenues vr)))
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
                        payable <- Account.subtract (Account.fromAmount (vatReportTotalVATRevenue vr)) (Account.fromAmount (vatReportPaidVAT vr))
                        pure $ vr {vatReportPayable = payable}
                    )
