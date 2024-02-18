{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Location.Gen where

import Centjes.Location
import Data.GenValidity
import Data.GenValidity.Path ()
import Numeric.DecimalLiteral.Gen ()
import Test.QuickCheck

instance (GenValid l, GenValid a) => GenValid (GenLocated l a)

instance GenValid SourceSpan

instance GenValid SourcePosition

genLocatedWith :: GenValid l => Gen a -> Gen (GenLocated l a)
genLocatedWith gen = Located <$> genValid <*> gen
