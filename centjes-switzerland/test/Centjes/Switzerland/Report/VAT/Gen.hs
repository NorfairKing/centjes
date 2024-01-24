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

instance GenValid Quarter

instance GenValid ann => GenValid (VATReport ann) where
  genValid =
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \vr -> do
                        totalRevenue <- Amount.add (vatReportForeignRevenue vr) (vatReportDomesticRevenue vr)
                        totalVATRevenue <- Amount.sum [vatReportStandardRateVAT81PercentRevenue vr]
                        payable <- Account.subtract (Account.fromAmount totalVATRevenue) (Account.fromAmount (vatReportPaidVAT vr))

                        pure $
                          vr
                            { vatReportTotalRevenue = totalRevenue,
                              vatReportTotalVATRevenue = totalVATRevenue,
                              vatReportPayable = payable
                            }
                    )
