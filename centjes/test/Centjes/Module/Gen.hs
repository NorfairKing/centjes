{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module.Gen where

import Centjes.Module
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import qualified Data.Text as T
import Money.Account.Gen ()
import Test.QuickCheck

instance GenValid Module

instance GenValid Declaration

instance GenValid Import

instance GenValid CurrencyDeclaration

instance GenValid CurrencySymbol where
  genValid =
    fmap CurrencySymbol $ do
      let genChar = choose ('A', 'Z')
      T.cons <$> genChar <*> genTextBy genChar

instance GenValid Transaction

instance GenValid Description where
  genValid =
    fmap Description $
      genTextBy $
        genValid `suchThat` (validationIsValid . validateDescriptionChar)

instance GenValid Posting

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
