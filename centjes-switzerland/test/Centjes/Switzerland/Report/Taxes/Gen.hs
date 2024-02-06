{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Gen where

import Centjes.Ledger.Gen ()
import Centjes.Switzerland.Report.Taxes.Types
import Data.GenValidity
import Data.GenValidity.Time ()
import Money.Amount.Gen ()

instance GenValid TaxesInput

instance (Show ann, Ord ann, GenValid ann) => GenValid (TaxesReport ann) where
  genValid =
    genValidStructurallyWithoutExtraChecking
