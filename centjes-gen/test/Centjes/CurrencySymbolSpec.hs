{-# LANGUAGE TypeApplications #-}

module Centjes.CurrencySymbolSpec (spec) where

import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.CurrencySymbol.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @CurrencySymbol

  describe "fromText" $
    it "produces valid currency symbols" $
      producesValid CurrencySymbol.fromText
