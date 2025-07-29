{-# LANGUAGE TypeApplications #-}

module Centjes.TimestampSpec (spec) where

import Centjes.Timestamp as Timestamp
import Centjes.Timestamp.Gen ()
import Data.Time
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

  describe "comparePartially" $ do
    it "compares two timestamps with only a day totally" $
      forAllValid $ \d1 ->
        forAllValid $ \d2 ->
          comparePartially (TimestampDay d1) (TimestampDay d2) `shouldBe` Just (compare d1 d2)

    it "compares two timestamps with minutes totally" $
      forAllValid $ \d ->
        forAllValid $ \m1 ->
          forAllValid $ \m2 ->
            comparePartially (TimestampMinute d m1) (TimestampMinute d m2) `shouldBe` Just (compare m1 m2)

    it "compares two timestamps with seconds totally" $
      forAllValid $ \d ->
        forAllValid $ \s1 ->
          forAllValid $ \s2 ->
            comparePartially (TimestampSecond d s1) (TimestampSecond d s2) `shouldBe` Just (compare s1 s2)

    it "compares 00:00 and 00:01 on the same day correctly" $
      comparePartially
        (TimestampMinute (fromGregorian 2025 01 01) (MinuteOfDay 0))
        (TimestampMinute (fromGregorian 2025 01 01) (MinuteOfDay 1))
        `shouldBe` Just LT
