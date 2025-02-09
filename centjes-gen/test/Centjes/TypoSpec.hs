{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.TypoSpec (spec) where

import Centjes.Typo as Typo
import Data.GenValidity.Text ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "isTypoOf" $ do
    it "considers bar a typo of baz" $
      "bar" `shouldSatisfy` (`isTypoOf` "baz")
    it "considers bAr a typo of bar" $
      "bAr" `shouldSatisfy` (`isTypoOf` "bar")
    it "considers br a typo of bar" $
      "br" `shouldSatisfy` (`isTypoOf` "bar")
    it "considers baar a typo of bar" $
      "baar" `shouldSatisfy` (`isTypoOf` "bar")
    it "it does not consider EUR a typo of URA" $
      "EUR" `shouldNotSatisfy` (`isTypoOf` "URA")
    it "it does not consider USD a typo of AUD" $
      "USD" `shouldNotSatisfy` (`isTypoOf` "AUD")

    it "is not reflexive" $
      forAllValid $ \t ->
        isTypoOf t t

    it "is symmetrical" $
      forAllValid $ \t1 ->
        forAllValid $ \t2 ->
          isTypoOf t1 t2 == isTypoOf t2 t1
