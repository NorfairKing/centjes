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
