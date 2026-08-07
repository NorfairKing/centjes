{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A payout out of the Stripe balance.
--
-- The one thing this importer reads one at a time rather than as a monthly total, because
-- the other leg of each payout is a single line on a bank statement and the bank importer
-- books those one at a time.
module Centjes.Stripe.Payout
  ( PayoutId (..),
    PayoutStatus (..),
    payoutStatusMovedMoney,
    Payout (..),
    payoutDay,
  )
where

import Autodocodec
import Centjes.Stripe.Currency
import Centjes.Stripe.Timestamp
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time (Day)
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype PayoutId = PayoutId {unPayoutId :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity PayoutId

instance HasCodec PayoutId where
  codec = dimapCodec PayoutId unPayoutId codec

-- | How far along a payout is.
--
-- Enumerated rather than kept as text, because whether a payout moved any money is
-- decided from it.  A status Stripe adds later fails to decode, which is the right
-- failure: it stops a month rather than guessing which way a new status goes.
data PayoutStatus
  = PayoutStatusPaid
  | PayoutStatusPending
  | PayoutStatusInTransit
  | PayoutStatusCanceled
  | PayoutStatusFailed
  deriving stock (Show, Eq, Generic)

instance Validity PayoutStatus

instance HasCodec PayoutStatus where
  codec =
    stringConstCodec
      ( (PayoutStatusPaid, "paid")
          :| [ (PayoutStatusPending, "pending"),
               (PayoutStatusInTransit, "in_transit"),
               (PayoutStatusCanceled, "canceled"),
               (PayoutStatusFailed, "failed")
             ]
      )

-- | Whether a payout took money out of the Stripe balance.
--
-- Stripe deducts when the payout is created, so one merely on its way has already left.
-- A failed or canceled payout has its money returned, so booking it would move money in
-- the ledger that is still at Stripe.  The month's reconciliation is the backstop for the
-- case this cannot see: a payout that failed in a later month than it was created in.
payoutStatusMovedMoney :: PayoutStatus -> Bool
payoutStatusMovedMoney = \case
  PayoutStatusPaid -> True
  PayoutStatusPending -> True
  PayoutStatusInTransit -> True
  PayoutStatusCanceled -> False
  PayoutStatusFailed -> False

data Payout = Payout
  { payoutId :: !PayoutId,
    -- | When the money left the Stripe balance.
    payoutCreated :: !StripeTimestamp,
    payoutCurrency :: !StripeCurrency,
    -- | In minor units, positive, being money out.
    payoutAmount :: !Integer,
    payoutStatus :: !PayoutStatus
  }
  deriving stock (Show, Eq, Generic)

instance Validity Payout

instance HasCodec Payout where
  codec =
    object "Payout" $
      Payout
        <$> requiredField "id" "identifier" .= payoutId
        <*> requiredField "created" "when the money left the balance" .= payoutCreated
        <*> requiredField "currency" "currency of the amount" .= payoutCurrency
        <*> requiredField "amount" "amount in minor units" .= payoutAmount
        <*> requiredField "status" "how far along this payout is" .= payoutStatus

payoutDay :: Payout -> Day
payoutDay = stripeTimestampDay . payoutCreated
