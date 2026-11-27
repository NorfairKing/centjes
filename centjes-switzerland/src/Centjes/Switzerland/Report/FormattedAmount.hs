module Centjes.Switzerland.Report.FormattedAmount
  ( FormattedAmount (..),
    formatAmount,
  )
where

import Autodocodec
import Centjes.Ledger
import Centjes.Location
import qualified Money.Amount as Amount
import qualified Money.Amount as Money (Amount)

-- | An amount as the typst templates show it.
newtype FormattedAmount = FormattedAmount {unFormattedAmount :: String}

instance HasCodec FormattedAmount where
  codec = dimapCodec FormattedAmount unFormattedAmount codec

formatAmount :: Currency ann -> Money.Amount -> FormattedAmount
formatAmount currency amount =
  let Located _ qf = currencyQuantisationFactor currency
   in FormattedAmount (Amount.format qf amount)
