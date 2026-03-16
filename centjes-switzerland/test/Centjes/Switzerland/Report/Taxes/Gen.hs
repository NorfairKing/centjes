{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.Taxes.Types
import Control.Monad
import Data.GenValidity
import Data.GenValidity.Time ()
import Money.Amount (Amount)
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck

instance GenValid TaxesInput

instance GenValid PartitionedExpenseAccounts

fixPartitionedExpensesTotals ::
  (a -> Amount) ->
  PartitionedExpenses a ann ->
  Maybe (PartitionedExpenses a ann)
fixPartitionedExpensesTotals businessAccessor pe = do
  totalBusiness <- Amount.sum (map businessAccessor (partitionedExpensesBusinessExpenses pe))
  totalPrivate <- Amount.sum (map privateExpenseCHFAmount (partitionedExpensesPrivateExpenses pe))
  pure $
    pe
      { partitionedExpensesTotalBusinessExpenses = totalBusiness,
        partitionedExpensesTotalPrivateExpenses = totalPrivate
      }

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
                        fixed <- fixPartitionedExpensesTotals insuranceExpenseCHFAmount (taxesReportInsuranceExpenses tr)
                        pure $ tr {taxesReportInsuranceExpenses = fixed}
                    )
      `suchThatMap` ( \tr -> do
                        fixed <- fixPartitionedExpensesTotals homeofficeExpenseCHFAmount (taxesReportHomeofficeExpenses tr)
                        pure $ tr {taxesReportHomeofficeExpenses = fixed}
                    )
      `suchThatMap` ( \tr -> do
                        fixed <- fixPartitionedExpensesTotals electricityExpenseCHFAmount (taxesReportElectricityExpenses tr)
                        pure $ tr {taxesReportElectricityExpenses = fixed}
                    )
      `suchThatMap` ( \tr -> do
                        fixed <- fixPartitionedExpensesTotals phoneExpenseCHFAmount (taxesReportPhoneExpenses tr)
                        pure $ tr {taxesReportPhoneExpenses = fixed}
                    )
      `suchThatMap` ( \tr -> do
                        fixed <- fixPartitionedExpensesTotals travelExpenseCHFAmount (taxesReportTravelExpenses tr)
                        pure $ tr {taxesReportTravelExpenses = fixed}
                    )
      `suchThatMap` ( \tr -> do
                        fixed <- fixPartitionedExpensesTotals internetExpenseCHFAmount (taxesReportInternetExpenses tr)
                        pure $ tr {taxesReportInternetExpenses = fixed}
                    )
      `suchThatMap` ( \tr -> do
                        fixed <- fixHealthCostsTotals (taxesReportHealthCosts tr)
                        pure $ tr {taxesReportHealthCosts = fixed}
                    )

fixHealthCostsTotals ::
  HealthCosts ann ->
  Maybe (HealthCosts ann)
fixHealthCostsTotals hc = do
  totalInsurancePremiums <- Amount.sum (map healthExpenseCHFAmount (healthCostsInsurancePremiums hc))
  totalOther <- Amount.sum (map healthExpenseCHFAmount (healthCostsOther hc))
  totalDentist <- Amount.sum (map healthExpenseCHFAmount (healthCostsDentist hc))
  totalDoctor <- Amount.sum (map healthExpenseCHFAmount (healthCostsDoctor hc))
  totalHospital <- Amount.sum (map healthExpenseCHFAmount (healthCostsHospital hc))
  totalTherapy <- Amount.sum (map healthExpenseCHFAmount (healthCostsTherapy hc))
  pure $
    hc
      { healthCostsTotalInsurancePremiums = totalInsurancePremiums,
        healthCostsTotalOther = totalOther,
        healthCostsTotalDentist = totalDentist,
        healthCostsTotalDoctor = totalDoctor,
        healthCostsTotalHospital = totalHospital,
        healthCostsTotalTherapy = totalTherapy
      }

instance (Show ann, Ord ann, GenValid ann) => GenValid (HealthCosts ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

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

instance (Show ann, Ord ann, GenValid ann) => GenValid (DepreciationPurchase ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { depreciationPurchaseAmount = amount
        }

instance (Show ann, Ord ann, GenValid ann) => GenValid (PrivateExpense ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    amount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    chfAmount <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { privateExpenseAmount = amount,
          privateExpenseCHFAmount = chfAmount
        }

instance (GenValid a, Show ann, Ord ann, GenValid ann) => GenValid (PartitionedExpenses a ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance (Show ann, Ord ann, GenValid ann) => GenValid (DepreciationSchedule ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    r <- genValidStructurallyWithoutExtraChecking
    openingBalance <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    closingBalance <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    totalPurchases <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    depreciation <- Amount.fromMinimalQuantisations <$> choose (0, 100_000_000_00)
    pure $
      r
        { depreciationScheduleOpeningBalance = openingBalance,
          depreciationScheduleTotalPurchases = totalPurchases,
          depreciationScheduleDepreciation = depreciation,
          depreciationScheduleClosingBalance = closingBalance
        }
