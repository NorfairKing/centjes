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
        fmap T.strip $
          genTextBy $
            genValid `suchThat` (\char -> char /= '\n' && char /= '\r' && char /= '\f')
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
