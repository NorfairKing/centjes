{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Timestamp.Gen where

import Centjes.Timestamp as Timestamp
import Data.GenValidity
import Data.GenValidity.Time ()
import Test.QuickCheck

instance GenValid Timestamp

instance GenValid MinuteOfDay where
  genValid = MinuteOfDay <$> choose (0, 24 * 60 - 1)

instance GenValid SecondOfDay where
  genValid = SecondOfDay <$> choose (0, 24 * 60 * 60 - 1)
