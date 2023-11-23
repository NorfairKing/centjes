{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Location.Gen where

import Centjes.DecimalLiteral.Gen ()
import Centjes.Location
import Data.GenValidity

instance (GenValid l, GenValid a) => GenValid (GenLocated l a)

instance GenValid SourceSpan

instance GenValid SourcePosition
