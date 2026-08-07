{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Stripe.TimestampSpec (spec) where

import Centjes.Stripe.Gen (codecSpec)
import Centjes.Stripe.Timestamp
import Data.Time.Calendar (fromGregorian)
import Test.Syd

spec :: Spec
spec = do
  codecSpec @StripeTimestamp

  describe "stripeTimestampDay" $
    it "is the UTC day of the epoch second" $
      -- 2026-07-31T23:59:59Z
      stripeTimestampDay (StripeTimestamp 1_785_542_399) `shouldBe` fromGregorian 2026 7 31

  describe "stripeTimestampAtMidnight" $
    it "is the start of the day, which is how every interval is bounded" $
      stripeTimestampAtMidnight (fromGregorian 2026 8 1) `shouldBe` StripeTimestamp 1_785_542_400
