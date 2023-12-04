{-# LANGUAGE TypeApplications #-}

module Centjes.CurrencySymbolSpec (spec) where

import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.CurrencySymbol.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @CurrencySymbol

  describe "fromTextM" $
    it "does the same as fromText" $
      forAllValid $ \t ->
        CurrencySymbol.fromText t `shouldBe` CurrencySymbol.fromTextM t

  describe "fromText" $
    it "produces valid currency symbols" $
      producesValid CurrencySymbol.fromText
