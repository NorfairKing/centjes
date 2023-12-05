{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Description.Gen where

import Centjes.Description as Description
import Data.GenValidity
import Data.GenValidity.Text
import Test.QuickCheck

instance GenValid Description where
  genValid =
    genTextBy (genValid `suchThat` (validationIsValid . validateDescriptionChar))
      `suchThatMap` Description.fromText
