{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.AccountName.Gen where

import Centjes.DecimalLiteral.Gen ()
import Centjes.Location.Gen ()
import Centjes.Module
import Data.GenValidity
import Data.GenValidity.Text
import qualified Data.Text as T
import Test.QuickCheck

instance GenValid AccountName where
  genValid =
    fmap AccountName $ do
      let alpha =
            oneof
              [ choose ('a', 'z'),
                choose ('A', 'Z')
              ]
      let rest =
            oneof
              [ choose ('a', 'z'),
                choose ('A', 'Z'),
                choose ('0', '9'),
                elements [':', '_']
              ]
      T.cons <$> alpha <*> genTextBy rest
