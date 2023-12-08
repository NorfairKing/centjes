{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module.Gen where

import Centjes.CurrencySymbol.Gen ()
import Centjes.Description.Gen ()
import Centjes.Location.Gen ()
import Centjes.Module
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import qualified Data.Text as T
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

instance GenValid ann => GenValid (Transaction ann)

instance GenValid ann => GenValid (TransactionExtra ann)

instance GenValid Timestamp

instance GenValid ann => GenValid (Posting ann)

instance GenValid AccountName where
  genValid =
    fmap AccountName $ do
      let alpha =
            oneof
              [ choose ('a', 'z'),
                choose ('A', 'Z')
              ]
      let rest =
            oneof
              [ choose ('a', 'z'),
                choose ('A', 'Z'),
                choose ('0', '9'),
                elements [':', '_']
              ]
      T.cons <$> alpha <*> genTextBy rest

instance GenValid Attachment

instance GenValid ann => GenValid (Assertion ann)
