{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.Taxes.Types
import Data.GenValidity
import Data.GenValidity.Time ()
import qualified Money.Account as Account
import qualified Money.Amount as Amount
import Money.Amount.Gen ()
import Test.QuickCheck

instance GenValid TaxesInput

instance (Show ann, Ord ann, GenValid ann) => GenValid (TaxesReport ann) where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = do
    genValidStructurallyWithoutExtraChecking
      `suchThatMap` ( \tr -> do
                        totalAssets <- Account.sum (map assetAccountConvertedBalance (taxesReportAssetAccounts tr))
                        pure $ tr {taxesReportTotalAssets = totalAssets}
                    )
      `suchThatMap` ( \tr -> do
                        totalRevenues <- Amount.sum (map revenueAmount (taxesReportRevenues tr))
                        pure $ tr {taxesReportTotalRevenues = totalRevenues}
                    )

instance (Show ann, Ord ann, GenValid ann) => GenValid (AssetAccount ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (Show ann, Ord ann, GenValid ann) => GenValid (Revenue ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
