{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.CurrencySymbol.Gen where

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
import Test.QuickCheck

instance GenValid CurrencySymbol where
  genValid =
    fmap CurrencySymbol $ do
      let genChar = choose ('A', 'Z')
      T.cons <$> genChar <*> genTextBy genChar
