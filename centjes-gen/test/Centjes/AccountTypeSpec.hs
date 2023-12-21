{-# LANGUAGE TypeApplications #-}

module Centjes.AccountTypeSpec (spec) where

import Centjes.AccountType as AccountType
import Centjes.AccountType.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @AccountType
  describe "fromText" $
    it "roundtrips with toText" $
      forAllValid $ \an ->
        AccountType.fromText (AccountType.toText an) `shouldBe` Just an
