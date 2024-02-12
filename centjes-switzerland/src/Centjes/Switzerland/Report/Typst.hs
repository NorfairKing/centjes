{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Switzerland.Report.Typst
  ( AmountWithCurrency,
    amountToAmountWithCurrency,
    FormattedAmount,
    formatAmount,
  )
where

import Autodocodec
import Centjes.Ledger
import Centjes.Location
import Data.Aeson (FromJSON, ToJSON)
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount

data AmountWithCurrency = AmountWithCurrency
  { amountWithCurrencyAmount :: FormattedAmount,
    amountWithCurrencyCurrency :: CurrencySymbol
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec AmountWithCurrency)

instance HasCodec AmountWithCurrency where
  codec =
    object "AmountWithCurrency" $
      AmountWithCurrency
        <$> requiredField "formatted" "formatted amount"
          .= amountWithCurrencyAmount
        <*> requiredField "symbol" "currency symbol"
          .= amountWithCurrencyCurrency

amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
amountToAmountWithCurrency currency amount =
  let Located _ qf = currencyQuantisationFactor currency
      symbol = currencySymbol currency
   in AmountWithCurrency (Amount.format qf amount) symbol

type FormattedAmount = String

formatAmount :: Currency ann -> Money.Amount -> FormattedAmount
formatAmount currency account =
  let Located _ qf = currencyQuantisationFactor currency
   in Amount.format qf account
