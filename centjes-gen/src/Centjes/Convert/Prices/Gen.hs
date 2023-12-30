{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Convert.Prices.Gen where

import Centjes.Convert.Prices
import Data.GenValidity
import Data.GenValidity.Containers ()
import Money.ConversionRate.Gen ()

instance (Show cur, Ord cur, GenValid cur) => GenValid (Prices cur)
