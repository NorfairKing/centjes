{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Location.Gen where

import Centjes.Location
import Control.Monad
import Data.GenValidity
import Data.GenValidity.Path ()
import Numeric.DecimalLiteral.Gen ()
import Test.QuickCheck

instance (GenValid l, GenValid a) => GenValid (GenLocated l a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SourceSpan where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SourcePosition where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genLocatedWith :: (GenValid l) => Gen a -> Gen (GenLocated l a)
genLocatedWith g = Located <$> genValid <*> g

genMLocatedWith :: (GenValid l) => Gen (Maybe a) -> Gen (Maybe (GenLocated l a))
genMLocatedWith g = do
  ma <- g
  forM ma $ \a ->
    Located <$> genValid <*> pure a
