{-# LANGUAGE TypeApplications #-}

module Centjes.Stripe.CurrencySpec (spec) where

import Centjes.Stripe.Currency
import Centjes.Stripe.Gen (codecSpec)
import Test.Syd

spec :: Spec
spec = codecSpec @StripeCurrency
