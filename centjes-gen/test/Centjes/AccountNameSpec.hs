{-# LANGUAGE TypeApplications #-}

module Centjes.AccountNameSpec (spec) where

import Centjes.AccountName as AccountName
import Centjes.AccountName.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @AccountName
  describe "fromText" $
    it "roundtrips with toText" $
      forAllValid $ \an ->
        AccountName.fromText (AccountName.toText an) `shouldBe` Just an
