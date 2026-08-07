{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Stripe.Gen (codecSpec) where

import Autodocodec (HasCodec)
import Autodocodec.Aeson (eitherDecodeJSONViaCodec, encodeJSONViaCodec)
import Centjes.Stripe.API
import Centjes.Stripe.Currency
import Centjes.Stripe.Payout
import Centjes.Stripe.Report
import Centjes.Stripe.Timestamp
import Data.Data (Proxy (..), Typeable, typeRep)
import Data.GenValidity ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Test.Syd
import Test.Syd.Validity

instance GenValid StripeCurrency

instance GenValid StripeTimestamp

instance GenValid ReportTypeId

instance GenValid ReportType

instance GenValid ReportRunId

instance GenValid ReportRunStatus

instance GenValid ReportRun

instance GenValid PayoutId

instance GenValid PayoutStatus

instance GenValid Payout

instance (GenValid a) => GenValid (StripeList a)

-- | The roundtrip every codec in this package has to satisfy.
--
-- A codec that is not the inverse of itself is a codec that has silently reinterpreted
-- a Stripe response.  Lives here with the generators it needs.
codecSpec :: forall a. (Show a, Eq a, Typeable a, GenValid a, HasCodec a) => Spec
codecSpec =
  describe (show (typeRep (Proxy @a))) $ do
    genValidSpec @a
    it "roundtrips through JSON" $
      forAllValid $ \(a :: a) ->
        eitherDecodeJSONViaCodec (encodeJSONViaCodec a) `shouldBe` Right a
