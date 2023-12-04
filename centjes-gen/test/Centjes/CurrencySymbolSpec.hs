{-# LANGUAGE TypeApplications #-}

module Centjes.CurrencySymbolSpec (spec) where

import Centjes.CurrencySymbol
import Centjes.CurrencySymbol.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @CurrencySymbol
