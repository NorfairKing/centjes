{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Convert.PriceGraph.Gen where

import Centjes.Convert.PriceGraph
import Data.GenValidity
import Data.GenValidity.Containers ()
import Money.ConversionRate.Gen ()

instance (Show priority, Ord priority, GenValid priority, Show cur, Ord cur, GenValid cur) => GenValid (PriceGraph priority cur)

instance (GenValid a) => GenValid (Direction a)
