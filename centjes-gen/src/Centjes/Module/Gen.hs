{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module.Gen where

import Centjes.AccountName.Gen ()
import Centjes.AccountType.Gen ()
import Centjes.CurrencySymbol.Gen ()
import Centjes.Description.Gen ()
import Centjes.Location.Gen ()
import Centjes.Module
import Centjes.Timestamp.Gen ()
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Money.Account.Gen ()
import Money.QuantisationFactor.Gen ()
import Numeric.DecimalLiteral.Gen ()
import Path
import Test.QuickCheck

instance GenValid ann => GenValid (Module ann)

instance GenValid ann => GenValid (Declaration ann)

instance GenValid Import where
  genValid = genValid `suchThatMap` (fmap Import . replaceExtension ".cent")
  shrinkValid _ = []

instance GenValid ann => GenValid (CurrencyDeclaration ann)

instance GenValid ann => GenValid (AccountDeclaration ann)

instance GenValid ann => GenValid (PriceDeclaration ann)

instance GenValid ann => GenValid (CostExpression ann)

instance GenValid ann => GenValid (PercentageExpression ann)

instance GenValid ann => GenValid (Transaction ann)

instance GenValid ann => GenValid (TransactionExtra ann)

instance GenValid ann => GenValid (Posting ann)

instance GenValid ann => GenValid (Attachment ann)

instance GenValid ann => GenValid (Assertion ann)
