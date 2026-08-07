{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Centjes.Stripe.Timestamp
  ( StripeTimestamp (..),
    stripeTimestampDay,
    stripeTimestampAtMidnight,
  )
where

import Autodocodec
import Data.Int (Int64)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)

-- | A Stripe timestamp: seconds since the epoch, exactly as Stripe sends it.
--
-- Stored as the number Stripe sent rather than as a 'UTCTime' so that decoding and
-- encoding are inverse.  A 'UTCTime' carries sub-second precision that no Stripe
-- response ever has.
newtype StripeTimestamp = StripeTimestamp {unStripeTimestamp :: Int64}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity StripeTimestamp

instance HasCodec StripeTimestamp where
  codec = dimapCodec StripeTimestamp unStripeTimestamp codec

-- | The UTC day a timestamp falls on.
--
-- Divided out rather than going through 'NominalDiffTime', which would need a
-- conversion that can silently truncate.  Flooring division is what makes a
-- pre-epoch timestamp land on the day it happened rather than the day after.
stripeTimestampDay :: StripeTimestamp -> Day
stripeTimestampDay (StripeTimestamp seconds) =
  addDays (toInteger seconds `div` 86400) (fromGregorian 1970 1 1)

-- | The start of a UTC day, which is how every interval this importer asks for is bounded.
stripeTimestampAtMidnight :: Day -> StripeTimestamp
stripeTimestampAtMidnight day =
  StripeTimestamp $ floor $ utcTimeToPOSIXSeconds $ UTCTime day 0
