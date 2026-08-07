{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Centjes.Stripe.Currency (StripeCurrency (..)) where

import Autodocodec
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

-- | A currency code as Stripe writes it, which is lowercase.
newtype StripeCurrency = StripeCurrency {unStripeCurrency :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity StripeCurrency

instance HasCodec StripeCurrency where
  codec = dimapCodec StripeCurrency unStripeCurrency codec
