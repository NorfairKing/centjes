{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module.Gen where

import Centjes.AccountName.Gen ()
import Centjes.AccountType.Gen ()
import Centjes.CurrencySymbol.Gen ()
import Centjes.Description.Gen ()
import Centjes.Location.Gen
import Centjes.Module
import Centjes.Tag.Gen ()
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

instance (GenValid ann) => GenValid (Module ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (Declaration ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (Import ann) where
  genValid = Import <$> genLocatedWith (genValid `suchThatMap` replaceExtension ".cent")
  shrinkValid _ = []

instance (GenValid ann) => GenValid (CurrencyDeclaration ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (AccountDeclaration ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (AccountExtra ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (AccountAssertion ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (TagDeclaration ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (PriceDeclaration ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (CostExpression ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (RatioExpression ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (RationalExpression ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (Transaction ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (TransactionExtra ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (Posting ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (ExtraAttachment ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (Attachment ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (ExtraAssertion ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (Assertion ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ann) => GenValid (ExtraTag ann) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
