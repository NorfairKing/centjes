{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.VAT.Typst
  ( Input (..),
    vatReportInput,
  )
where

import Autodocodec
import qualified Centjes.Description as Description
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.VAT.Types
import qualified Centjes.Timestamp as Timestamp
import Data.Aeson (ToJSON)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.Quarter
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import Path
import Text.Printf

vatReportInput :: VATReport ann -> Input
vatReportInput VATReport {..} =
  let inputPersonName = vatReportPersonName
      inputOrganisationName = vatReportOrganisationName
      inputVATId = vatReportVATId
      inputQuarter = vatReportQuarter
      inputRevenues =
        sortOn inputRevenueDay $
          concat
            [ map vatInputDomesticRevenue vatReportDomesticRevenues,
              map vatInputForeignRevenue vatReportExportsRevenues,
              map vatInputForeignRevenue vatReportForeignRevenues
            ]
      orZero a = if a == Amount.zero then Nothing else Just a
      inputExpenses = map vatInputDeductibleExpense vatReportDeductibleExpenses
      inputTotalRevenue = formatAmount vatReportCHF vatReportTotalRevenue
      inputTotalExportsRevenue = formatAmount vatReportCHF <$> orZero vatReportTotalExportsRevenue
      inputTotalForeignRevenue = formatAmount vatReportCHF <$> orZero vatReportTotalForeignRevenue
      inputTotalForeignDeductions = formatAmount vatReportCHF vatReportTotalForeignDeductions
      inputDomesticRevenue2024 = formatAmount vatReportCHF vatReportDomesticRevenue2024
      input2024StandardRateVATRevenue = formatAmount vatReportCHF vatReport2024StandardRateVATRevenue
      inputTotalDomesticRevenue = formatAmount vatReportCHF vatReportTotalDomesticRevenue
      inputTotalVATRevenue = formatAmount vatReportCHF vatReportTotalVATRevenue
      inputPaidVAT = formatAmount vatReportCHF vatReportPaidVAT
      inputTotalVATDeductions = formatAmount vatReportCHF vatReportTotalVATDeductions
      (inputPayable, inputReceivable) = case vatReportPayable of
        Positive a -> (Just (formatAmount vatReportCHF a), Nothing)
        Negative a -> (Nothing, Just (formatAmount vatReportCHF a))
   in Input {..}

-- Note that this is a separate type from the EMWST 'XMLReport' because there
-- is more information in the README than there is in the EMWST
data Input = Input
  { inputPersonName :: Text,
    inputOrganisationName :: Text,
    -- | 111.222.333
    inputVATId :: Text,
    inputQuarter :: !Quarter,
    inputRevenues :: ![InputRevenue],
    inputExpenses :: ![InputExpense],
    -- | 200
    --
    -- Total der vereinbarten bzw. vereinnahmten Entgelte, inkl.
    -- optierte Leistungen, Entgelte aus Übertragungen im
    -- Meldeverfahren sowie aus Leistungen im Ausland
    -- (weltweiter Umsatz)
    inputTotalRevenue :: !FormattedAmount,
    -- | 220
    --
    -- Leistungen ins Ausland
    inputTotalExportsRevenue :: !(Maybe FormattedAmount),
    -- | 221
    --
    -- Leistungen im Ausland (Ort der Leistung im Ausland)
    inputTotalForeignRevenue :: !(Maybe FormattedAmount),
    -- | 289
    --
    -- Totale AbZüge
    inputTotalForeignDeductions :: !FormattedAmount,
    -- | 299
    --
    -- Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)
    inputTotalDomesticRevenue :: !FormattedAmount,
    -- | 303
    --
    -- Leistungen zum Normalsatz 8.1%
    inputDomesticRevenue2024 :: !FormattedAmount,
    input2024StandardRateVATRevenue :: !FormattedAmount,
    -- | 399
    --
    -- Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)
    inputTotalVATRevenue :: !FormattedAmount,
    -- | 405
    --
    -- Vorsteuer auf Investitionen und übrigem Betriebsaufwand
    inputPaidVAT :: !FormattedAmount,
    -- | 479
    --
    -- Totale Abzuge
    inputTotalVATDeductions :: !FormattedAmount,
    -- | 500
    --
    -- Zu bezahlender Betrag
    inputPayable :: !(Maybe FormattedAmount),
    -- | 510
    --
    --  Guthaben der steuerpflichtigen Person
    inputReceivable :: !(Maybe FormattedAmount)
  }
  deriving (ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "person_name" "person name"
          .= inputPersonName
        <*> requiredField "organisation_name" "organisation name"
          .= inputOrganisationName
        <*> requiredField "vat_id" "VAT Identifier. e.g. 111.222.333"
          .= inputVATId
        <*> requiredFieldWith "quarter" (codecViaAeson "Quarter") "quarter"
          .= inputQuarter
        <*> requiredField "revenues" "revenues"
          .= inputRevenues
        <*> requiredField "expenses" "expenses"
          .= inputExpenses
        <*> requiredField "total_revenue" "total_revenue"
          .= inputTotalRevenue
        <*> requiredField "total_exports_revenue" "total exports revenue"
          .= inputTotalExportsRevenue
        <*> requiredField "total_foreign_revenue" "total foreign revenue"
          .= inputTotalForeignRevenue
        <*> requiredField "total_foreign_deductions" "total foreign deductions"
          .= inputTotalForeignDeductions
        <*> requiredField "total_domestic_revenue" "total domestic revenue"
          .= inputTotalDomesticRevenue
        <*> requiredField "domestic_revenue_2024" "domestic revenue from 2024"
          .= inputDomesticRevenue2024
        <*> requiredField "vat_revenue_standard_2024" "vat_standard"
          .= input2024StandardRateVATRevenue
        <*> requiredField "total_vat_revenue" "total vat"
          .= inputTotalVATRevenue
        <*> requiredField "vat_paid" "total vat"
          .= inputPaidVAT
        <*> requiredField "total_vat_deductions" "total vat deductions"
          .= inputTotalVATDeductions
        <*> requiredField "payable" "payable"
          .= inputPayable
        <*> requiredField "receivable" "receivable"
          .= inputReceivable

vatInputDomesticRevenue :: DomesticRevenue ann -> InputRevenue
vatInputDomesticRevenue DomesticRevenue {..} =
  let inputRevenueDay = Timestamp.toDay domesticRevenueTimestamp
      inputRevenueDescription = Description.toText domesticRevenueDescription
      inputRevenueAmount = amountToAmountWithCurrency domesticRevenueCurrency domesticRevenueAmount
      inputRevenueCHFAmount = formatAmount domesticRevenueCurrency domesticRevenueCHFAmount
      inputRevenueVATAmount = Just $ amountToAmountWithCurrency domesticRevenueVATCurrency domesticRevenueVATAmount
      inputRevenueVATCHFAmount = Just $ formatAmount domesticRevenueVATCurrency domesticRevenueVATCHFAmount
      inputRevenueVATRate = Just $ formatVATRate domesticRevenueVATRate
      inputRevenueEvidence = domesticRevenueEvidence
   in InputRevenue {..}

vatInputForeignRevenue :: ForeignRevenue ann -> InputRevenue
vatInputForeignRevenue ForeignRevenue {..} =
  let inputRevenueDay = Timestamp.toDay foreignRevenueTimestamp
      inputRevenueDescription = Description.toText foreignRevenueDescription
      inputRevenueAmount = amountToAmountWithCurrency foreignRevenueCurrency foreignRevenueAmount
      inputRevenueCHFAmount = formatAmount foreignRevenueCurrency foreignRevenueCHFAmount
      inputRevenueVATAmount = Nothing
      inputRevenueVATCHFAmount = Nothing
      inputRevenueVATRate = Nothing
      inputRevenueEvidence = foreignRevenueEvidence
   in InputRevenue {..}

vatInputDeductibleExpense :: DeductibleExpense ann -> InputExpense
vatInputDeductibleExpense DeductibleExpense {..} =
  let inputExpenseDay = Timestamp.toDay deductibleExpenseTimestamp
      inputExpenseDescription = Description.toText deductibleExpenseDescription
      inputExpenseAmount = amountToAmountWithCurrency deductibleExpenseCurrency deductibleExpenseAmount
      inputExpenseCHFAmount = formatAmount deductibleExpenseCurrency deductibleExpenseCHFAmount
      inputExpenseVATAmount = amountToAmountWithCurrency deductibleExpenseVATCurrency deductibleExpenseVATAmount
      inputExpenseVATCHFAmount = formatAmount deductibleExpenseVATCurrency deductibleExpenseVATCHFAmount
      inputExpenseVATRate = printf "%.1f %%" (realToFrac (deductibleExpenseVATRate * 100) :: Double)
      inputExpenseEvidence = deductibleExpenseEvidence
   in InputExpense {..}

formatVATRate :: VATRate -> String
formatVATRate = \case
  VATRate2024Standard -> "8.1 %"
  VATRate2024Reduced -> "2.6 %"
  VATRate2024Hotel -> "3.8 %"

data InputRevenue = InputRevenue
  { inputRevenueDay :: !Day,
    inputRevenueDescription :: !Text,
    inputRevenueAmount :: !AmountWithCurrency,
    inputRevenueCHFAmount :: !FormattedAmount,
    inputRevenueVATAmount :: !(Maybe AmountWithCurrency),
    inputRevenueVATCHFAmount :: !(Maybe FormattedAmount),
    inputRevenueVATRate :: !(Maybe String),
    inputRevenueEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec InputRevenue where
  codec =
    object "InputRevenue" $
      InputRevenue
        <$> requiredField "day" "day of revenue"
          .= inputRevenueDay
        <*> requiredField "description" "description of revenue"
          .= inputRevenueDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputRevenueAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputRevenueCHFAmount
        <*> optionalField "vat_amount" "VAT amount in original currency"
          .= inputRevenueVATAmount
        <*> optionalField "vat_amount_chf" "VAT amount in chf"
          .= inputRevenueVATCHFAmount
        <*> optionalField "vat_rate" "VAT rate"
          .= inputRevenueVATRate
        <*> requiredField "evidence" "evidence"
          .= inputRevenueEvidence

data InputExpense = InputExpense
  { inputExpenseDay :: !Day,
    inputExpenseDescription :: !Text,
    inputExpenseAmount :: !AmountWithCurrency,
    inputExpenseCHFAmount :: !FormattedAmount,
    inputExpenseVATAmount :: !AmountWithCurrency,
    inputExpenseVATCHFAmount :: !FormattedAmount,
    inputExpenseVATRate :: !String,
    inputExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec InputExpense where
  codec =
    object "InputExpense" $
      InputExpense
        <$> requiredField "day" "day of expense"
          .= inputExpenseDay
        <*> requiredField "description" "description of expense"
          .= inputExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputExpenseCHFAmount
        <*> requiredField "vat_amount" "VAT amount in original currency"
          .= inputExpenseVATAmount
        <*> requiredField "vat_amount_chf" "VAT amount in chf"
          .= inputExpenseVATCHFAmount
        <*> requiredField "vat_rate" "VAT rate"
          .= inputExpenseVATRate
        <*> requiredField "evidence" "evidence"
          .= inputExpenseEvidence

data AmountWithCurrency = AmountWithCurrency
  { amountWithCurrencyAmount :: FormattedAmount,
    amountWithCurrencyCurrency :: CurrencySymbol
  }

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
