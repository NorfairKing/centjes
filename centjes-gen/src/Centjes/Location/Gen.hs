{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Location.Gen where

import Centjes.Location
import Data.GenValidity
import Data.GenValidity.Path ()
import Numeric.DecimalLiteral.Gen ()

instance (GenValid l, GenValid a) => GenValid (GenLocated l a)

instance GenValid SourceSpan

instance GenValid SourcePosition
