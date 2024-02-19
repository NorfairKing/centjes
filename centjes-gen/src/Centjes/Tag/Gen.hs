{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Tag.Gen where

import Centjes.Location.Gen ()
import Centjes.Tag
import Data.GenValidity
import Data.GenValidity.Text
import qualified Data.Text as T
import Test.QuickCheck

instance GenValid Tag where
  genValid =
    fmap Tag $ do
      let genChar = choose ('A', 'Z')
      T.cons <$> genChar <*> genTextBy genChar
