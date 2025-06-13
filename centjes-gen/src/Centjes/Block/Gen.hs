{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Block.Gen where

import Centjes.Block
import Data.GenValidity

instance GenValid BlockSize where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
