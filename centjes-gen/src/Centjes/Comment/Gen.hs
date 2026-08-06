{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Comment.Gen where

import Centjes.Comment
import Data.GenValidity
import Data.GenValidity.Text
import qualified Data.Text as T
import Test.QuickCheck

instance GenValid Comment where
  genValid =
    fmap (Comment . T.intercalate "\n") $
      genListOf $
        genTextBy $
          genValid `suchThat` (\char -> char /= '\n' && char /= '\r' && char /= '\f')
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
