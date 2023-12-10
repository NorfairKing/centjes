{-# LANGUAGE TypeApplications #-}

module Centjes.TimestampSpec (spec) where

import Centjes.Timestamp as Timestamp
import Centjes.Timestamp.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Timestamp
  describe "toText" $
    it "produces valid values" $
      producesValid Timestamp.toText

  describe "fromText" $
    it "roundtrips with toText" $
      forAllValid $ \ts -> do
        let rendered = toText ts
        context (show rendered) $ fromText rendered `shouldBe` Right ts

  describe "toString" $
    it "produces valid values" $
      producesValid Timestamp.toString
