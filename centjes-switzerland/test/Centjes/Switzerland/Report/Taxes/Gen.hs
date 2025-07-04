{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.Taxes.Types
import Control.Monad
import Data.GenValidity
import Data.GenValidity.Time ()
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck

instance GenValid TaxesInput

instance (Show ann, Ord ann, GenValid ann) => GenValid (TaxesReport ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \tr -> do
                        totalAssets <- Amount.sum (map assetAccountConvertedBalance (taxesReportAssetAccounts tr))
                        pure $ tr {taxesReportTotalAssets = totalAssets}
                    )
      `suchThatMap` ( \tr -> do
                        totalRevenues <- Amount.sum (map revenueAmount (taxesReportRevenues tr))
                        pure $ tr {taxesReportTotalRevenues = totalRevenues}
                    )
      `suchThatMap` ( \tr -> do
                        totalThirdPillarContributions <- Amount.sum (map thirdPillarContributionCHFAmount (taxesReportThirdPillarContributions tr))
                        pure $ tr {taxesReportTotalThirdPillarContributions = totalThirdPillarContributions}
                    )
      `suchThatMap` ( \tr -> do
                        totalHomeofficeExpenses <- Amount.sum (map homeofficeExpenseAmount (taxesReportHomeofficeExpenses tr))
                        pure $ tr {taxesReportTotalHomeofficeExpenses = totalHomeofficeExpenses}
                    )
      `suchThatMap` ( \tr -> do
                        totalInsuranceExpenses <- Amount.sum (map insuranceExpenseAmount (taxesReportInsuranceExpenses tr))
                        pure $ tr {taxesReportTotalInsuranceExpenses = totalInsuranceExpenses}
                    )
      `suchThatMap` ( \tr -> do
                        totalElectricityExpenses <- Amount.sum (map electricityExpenseAmount (taxesReportElectricityExpenses tr))
                        pure $ tr {taxesReportTotalElectricityExpenses = totalElectricityExpenses}
                    )
      `suchThatMap` ( \tr -> do
                        totalPhoneExpenses <- Amount.sum (map phoneExpenseAmount (taxesReportPhoneExpenses tr))
                        pure $ tr {taxesReportTotalPhoneExpenses = totalPhoneExpenses}
                    )
      `suchThatMap` ( \tr -> do
                        totalTravelExpenses <- Amount.sum (map travelExpenseAmount (taxesReportTravelExpenses tr))
                        pure $ tr {taxesReportTotalTravelExpenses = totalTravelExpenses}
                    )
      `suchThatMap` ( \tr -> do
                        totalInternetExpenses <- Amount.sum (map internetExpenseAmount (taxesReportInternetExpenses tr))
                        pure $ tr {taxesReportTotalInternetExpenses = totalInternetExpenses}
                    )
      `suchThatMap` ( \tr -> do
                        totalHealthExpenses <- Amount.sum (map healthExpenseAmount (taxesReportHealthExpenses tr))
                        pure $ tr {taxesReportTotalHealthExpenses = totalHealthExpenses}
                    )

instance (Show ann, Ord ann, GenValid ann) => GenValid (AssetAccount ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    a <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    balances <- forM (assetAccountBalances a) $ \_ -> do
      a1 <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
      a2 <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
      pure (a1, a2)
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      a
        { assetAccountBalances = balances,
          assetAccountConvertedBalance = amount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (Revenue ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { revenueAmount = amount,
          revenueCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (ThirdPillarContribution ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { thirdPillarContributionCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (InsuranceExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { insuranceExpenseAmount = amount,
          insuranceExpenseCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (HomeofficeExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { homeofficeExpenseAmount = amount,
          homeofficeExpenseCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (ElectricityExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { electricityExpenseAmount = amount,
          electricityExpenseCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (PhoneExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { phoneExpenseAmount = amount,
          phoneExpenseCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (TravelExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { travelExpenseAmount = amount,
          travelExpenseCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (InternetExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { internetExpenseAmount = amount,
          internetExpenseCHFAmount = chfAmount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (HealthExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    -- If this number is too small, that's a VERY good problem.
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { healthExpenseAmount = amount,
          healthExpenseCHFAmount = chfAmount
        }
