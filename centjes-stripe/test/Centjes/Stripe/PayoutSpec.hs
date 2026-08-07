{-# LANGUAGE TypeApplications #-}

module Centjes.Stripe.PayoutSpec (spec) where

import Centjes.Stripe.Gen (codecSpec)
import Centjes.Stripe.Payout
import Test.Syd

spec :: Spec
spec = do
  codecSpec @PayoutId
  codecSpec @PayoutStatus
  codecSpec @Payout

  describe "payoutStatusMovedMoney" $ do
    it "counts a payout on its way as gone, because Stripe deducts on creation" $
      map payoutStatusMovedMoney [PayoutStatusPaid, PayoutStatusPending, PayoutStatusInTransit]
        `shouldBe` [True, True, True]

    it "counts a payout that came back as never gone" $
      map payoutStatusMovedMoney [PayoutStatusCanceled, PayoutStatusFailed]
        `shouldBe` [False, False]
