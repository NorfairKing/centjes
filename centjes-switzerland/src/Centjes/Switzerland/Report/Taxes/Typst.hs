{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes.Typst
  ( Input (..),
    taxesReportInput,
  )
where

import Autodocodec
import qualified Centjes.Description as Description
import Centjes.Ledger
import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.Report.Typst
import Centjes.Switzerland.Report.VATRate
import qualified Centjes.Timestamp as Timestamp
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time
import Path

taxesReportInput :: TaxesReport ann -> Input
taxesReportInput TaxesReport {..} =
  let inputLastName = taxesReportLastName
      inputFirstName = taxesReportFirstName
      inputYear = taxesReportYear
      inputRevenues = map (taxesInputRevenue taxesReportCHF) taxesReportRevenues
      inputSelfEmploymentRevenue = formatAmount taxesReportCHF taxesReportSelfEmploymentRevenue
   in Input {..}

-- Note that this is a separate type from the ETax 'XMLReport' because there
-- is more information in the README than there is in the ETax
data Input = Input
  { inputLastName :: Text,
    inputFirstName :: Text,
    inputYear :: !Year,
    inputRevenues :: ![InputRevenue],
    inputSelfEmploymentRevenue :: !FormattedAmount
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "last_name" "last name"
          .= inputLastName
        <*> requiredField "first_name" "first name"
          .= inputFirstName
        <*> requiredField "year" "year"
          .= inputYear
        <*> requiredField "revenues" "revenues"
          .= inputRevenues
        <*> requiredField "self_employment_revenue" "self employment revenue"
          .= inputSelfEmploymentRevenue

data InputRevenue = InputRevenue
  { inputRevenueDay :: !Day,
    inputRevenueDescription :: !Text,
    inputRevenueGrossAmount :: !AmountWithCurrency,
    inputRevenueGrossCHFAmount :: !FormattedAmount,
    inputRevenueVATAmount :: !(Maybe AmountWithCurrency),
    inputRevenueVATCHFAmount :: !(Maybe FormattedAmount),
    inputRevenueVATRate :: !(Maybe String),
    inputRevenueNettoCHFAmount :: !FormattedAmount,
    inputRevenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec InputRevenue)

instance HasCodec InputRevenue where
  codec =
    object "InputRevenue" $
      InputRevenue
        <$> requiredField "day" "day of revenue"
          .= inputRevenueDay
        <*> requiredField "description" "description of revenue"
          .= inputRevenueDescription
        <*> requiredField "gross_amount" "amount in original currency"
          .= inputRevenueGrossAmount
        <*> requiredField "gross_amount_chf" "amount in chf"
          .= inputRevenueGrossCHFAmount
        <*> optionalField "vat_amount" "VAT amount in original currency"
          .= inputRevenueVATAmount
        <*> optionalField "vat_amount_chf" "VAT amount in chf"
          .= inputRevenueVATCHFAmount
        <*> optionalField "vat_rate" "VAT rate"
          .= inputRevenueVATRate
        <*> requiredField "netto_amount" "netto amount in chf currency"
          .= inputRevenueNettoCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputRevenueEvidence

taxesInputRevenue :: Currency ann -> Revenue ann -> InputRevenue
taxesInputRevenue chfCurrency Revenue {..} =
  let inputRevenueDay = Timestamp.toDay revenueTimestamp
      inputRevenueDescription = Description.toText revenueDescription
      inputRevenueGrossAmount = amountToAmountWithCurrency revenueCurrency revenueGrossAmount
      inputRevenueGrossCHFAmount = formatAmount chfCurrency revenueGrossCHFAmount
      inputRevenueVATAmount = amountToAmountWithCurrency revenueCurrency . vatRevenueAmount <$> revenueVAT
      inputRevenueVATCHFAmount = formatAmount chfCurrency . vatRevenueCHFAmount <$> revenueVAT
      inputRevenueVATRate = formatVATRate . vatRevenueVATRate <$> revenueVAT
      inputRevenueNettoCHFAmount = formatAmount chfCurrency revenueNettoCHFAmount
      inputRevenueEvidence = revenueEvidence
   in InputRevenue {..}
