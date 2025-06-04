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
import qualified Centjes.CurrencySymbol as CurrencySymbol
import qualified Centjes.Description as Description
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.Taxes.Types
import qualified Centjes.Timestamp as Timestamp
import Data.Aeson (ToJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Text (Text)
import Data.Time
import qualified Money.Amount as Amount
import qualified Money.ConversionRate as ConversionRate
import Numeric.Natural
import Path
import Text.Printf

taxesReportInput :: TaxesReport ann -> Input
taxesReportInput TaxesReport {..} =
  let formatAmount cur = Amount.format (locatedValue (currencyQuantisationFactor cur))
      formatChfAmount = formatAmount taxesReportCHF

      inputLastName = taxesReportLastName
      inputFirstName = taxesReportFirstName
      inputYear = taxesReportYear
      inputConversionRates =
        M.fromList
          $ map
            ( \(Currency {..}, rate) ->
                ( CurrencySymbol.toText currencySymbol,
                  printf "%.4f" $ (realToFrac :: Ratio Natural -> Double) $ ConversionRate.toRatio rate
                )
            )
          $ M.toList taxesReportConversionRates
      inputAssets =
        map
          ( \AssetAccount {..} ->
              let assetInputAccountName = assetAccountName
                  assetInputBalances =
                    M.fromList $
                      map
                        ( \(cur@Currency {..}, (amount, chfAmount)) ->
                            ( CurrencySymbol.toText currencySymbol,
                              Balance {balanceOriginal = formatAmount cur amount, balanceConverted = formatChfAmount chfAmount}
                            )
                        )
                        (M.toList assetAccountBalances)
                  assetInputConvertedBalance = formatChfAmount assetAccountConvertedBalance
                  assetInputEvidence = assetAccountAttachments
               in AssetInput {..}
          )
          taxesReportAssetAccounts
      inputTotalRevenues = formatChfAmount taxesReportTotalRevenues
      inputRevenues = flip map taxesReportRevenues $ \Revenue {..} ->
        let inputRevenueDay = Timestamp.toDay revenueTimestamp
            inputRevenueDescription = Description.toText revenueDescription
            inputRevenueAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount revenueCurrency revenueAmount,
                  amountWithCurrencyCurrency = currencySymbol revenueCurrency
                }
            inputRevenueCHFAmount = formatChfAmount revenueCHFAmount
            inputRevenueEvidence = revenueEvidence
         in RevenueInput {..}
      inputTotalAssets = formatChfAmount taxesReportTotalAssets
      inputHomeofficeExpenses = flip map taxesReportHomeofficeExpenses $ \HomeofficeExpense {..} ->
        let inputHomeofficeExpenseDay = Timestamp.toDay homeofficeExpenseTimestamp
            inputHomeofficeExpenseDescription = Description.toText homeofficeExpenseDescription
            inputHomeofficeExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount homeofficeExpenseCurrency homeofficeExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol homeofficeExpenseCurrency
                }
            inputHomeofficeExpenseCHFAmount = formatChfAmount homeofficeExpenseCHFAmount
            inputHomeofficeExpenseEvidence = homeofficeExpenseEvidence
         in HomeofficeExpenseInput {..}
      inputTotalHomeofficeExpenses = formatChfAmount taxesReportTotalHomeofficeExpenses
   in Input {..}

-- Note that this is a separate type from the ETax 'XMLReport' because there
-- is more information in the README than there is in the ETax
data Input = Input
  { inputLastName :: Text,
    inputFirstName :: Text,
    inputYear :: !Year,
    inputConversionRates :: !(Map Text String),
    inputAssets :: ![AssetInput],
    inputTotalAssets :: !FormattedAmount,
    inputRevenues :: ![RevenueInput],
    inputTotalRevenues :: !FormattedAmount,
    inputHomeofficeExpenses :: ![HomeofficeExpenseInput],
    inputTotalHomeofficeExpenses :: !FormattedAmount
  }
  deriving (ToJSON) via (Autodocodec Input)

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
        <*> requiredField "rates" "conversion rates"
          .= inputConversionRates
        <*> requiredField "assets" "assets"
          .= inputAssets
        <*> requiredField "total_assets" "total assets"
          .= inputTotalAssets
        <*> requiredField "revenues" "revenues"
          .= inputRevenues
        <*> requiredField "total_revenues" "total revenues"
          .= inputTotalRevenues
        <*> requiredField "homeoffice_expenses" "homeoffice expenses"
          .= inputHomeofficeExpenses
        <*> requiredField "total_homeoffice_expenses" "total homeoffice expenses"
          .= inputTotalHomeofficeExpenses

data AssetInput = AssetInput
  { assetInputAccountName :: !AccountName,
    assetInputBalances :: !(Map Text Balance),
    assetInputConvertedBalance :: !String,
    assetInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec AssetInput where
  codec =
    object "AssetInput" $
      AssetInput
        <$> requiredField "name" "name"
          .= assetInputAccountName
        <*> requiredField "balances" "balances"
          .= assetInputBalances
        <*> requiredField "balance" "balance"
          .= assetInputConvertedBalance
        <*> requiredField "evidence" "evidence"
          .= assetInputEvidence

data Balance = Balance
  { balanceOriginal :: String,
    balanceConverted :: String
  }

instance HasCodec Balance where
  codec =
    object "Balance" $
      Balance
        <$> requiredField "original" "balance in original currency" .= balanceOriginal
        <*> requiredField "converted" "balance in CHF" .= balanceConverted

data RevenueInput = RevenueInput
  { inputRevenueDay :: !Day,
    inputRevenueDescription :: !Text,
    inputRevenueAmount :: !AmountWithCurrency,
    inputRevenueCHFAmount :: !FormattedAmount,
    inputRevenueEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec RevenueInput where
  codec =
    object "RevenueInput" $
      RevenueInput
        <$> requiredField "day" "day of revenue"
          .= inputRevenueDay
        <*> requiredField "description" "description of revenue"
          .= inputRevenueDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputRevenueAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputRevenueCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputRevenueEvidence

data HomeofficeExpenseInput = HomeofficeExpenseInput
  { inputHomeofficeExpenseDay :: !Day,
    inputHomeofficeExpenseDescription :: !Text,
    inputHomeofficeExpenseAmount :: !AmountWithCurrency,
    inputHomeofficeExpenseCHFAmount :: !FormattedAmount,
    inputHomeofficeExpenseEvidence :: ![Path Rel File]
  }

instance HasCodec HomeofficeExpenseInput where
  codec =
    object "HomeofficeExpenseInput" $
      HomeofficeExpenseInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputHomeofficeExpenseDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputHomeofficeExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputHomeofficeExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputHomeofficeExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputHomeofficeExpenseEvidence

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

type FormattedAmount = String
