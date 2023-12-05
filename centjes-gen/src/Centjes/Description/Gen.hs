{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Description.Gen where

import Centjes.Description
import Data.GenValidity
import Data.GenValidity.Text
import Test.QuickCheck

instance GenValid Description where
  genValid =
    fmap Description $
      genTextBy $
        genValid `suchThat` (validationIsValid . validateDescriptionChar)
