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

instance GenValid StripeCurrency where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid StripeTimestamp where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ReportTypeId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ReportType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ReportRunId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ReportRunStatus where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ReportRun where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PayoutId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PayoutStatus where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Payout where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a) => GenValid (StripeList a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

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
