{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.DecimalLiteral.Gen where

import Centjes.DecimalLiteral
import Data.GenValidity
import Data.GenValidity.Scientific ()
import Data.Int
import Data.Scientific
import Test.QuickCheck

instance GenValid DecimalLiteral where
  -- We only generate small (in absolute value) Scientific values
  genValid = DecimalLiteral <$> (scientific <$> genValid <*> ((fromIntegral :: Int8 -> Int) <$> (genValid :: Gen Int8)))
