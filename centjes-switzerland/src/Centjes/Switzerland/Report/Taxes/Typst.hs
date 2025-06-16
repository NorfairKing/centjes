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
      inputThirdPillarContributions = flip map taxesReportThirdPillarContributions $ \ThirdPillarContribution {..} ->
        let inputThirdPillarContributionDay = Timestamp.toDay thirdPillarContributionTimestamp
            inputThirdPillarContributionDescription = Description.toText thirdPillarContributionDescription
            inputThirdPillarContributionCHFAmount = formatChfAmount thirdPillarContributionCHFAmount
            inputThirdPillarContributionEvidence = thirdPillarContributionEvidence
         in ThirdPillarContributionInput {..}
      inputTotalThirdPillarContributions = formatChfAmount taxesReportTotalThirdPillarContributions
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
      inputElectricityExpenses = flip map taxesReportElectricityExpenses $ \ElectricityExpense {..} ->
        let inputElectricityExpenseDay = Timestamp.toDay electricityExpenseTimestamp
            inputElectricityExpenseDescription = Description.toText electricityExpenseDescription
            inputElectricityExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount electricityExpenseCurrency electricityExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol electricityExpenseCurrency
                }
            inputElectricityExpenseCHFAmount = formatChfAmount electricityExpenseCHFAmount
            inputElectricityExpenseEvidence = electricityExpenseEvidence
         in ElectricityExpenseInput {..}
      inputTotalElectricityExpenses = formatChfAmount taxesReportTotalElectricityExpenses
      inputPhoneExpenses = flip map taxesReportPhoneExpenses $ \PhoneExpense {..} ->
        let inputPhoneExpenseDay = Timestamp.toDay phoneExpenseTimestamp
            inputPhoneExpenseDescription = Description.toText phoneExpenseDescription
            inputPhoneExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount phoneExpenseCurrency phoneExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol phoneExpenseCurrency
                }
            inputPhoneExpenseCHFAmount = formatChfAmount phoneExpenseCHFAmount
            inputPhoneExpenseEvidence = phoneExpenseEvidence
         in PhoneExpenseInput {..}
      inputTotalPhoneExpenses = formatChfAmount taxesReportTotalPhoneExpenses
      inputTravelExpenses = flip map taxesReportTravelExpenses $ \TravelExpense {..} ->
        let inputTravelExpenseDay = Timestamp.toDay travelExpenseTimestamp
            inputTravelExpenseDescription = Description.toText travelExpenseDescription
            inputTravelExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount travelExpenseCurrency travelExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol travelExpenseCurrency
                }
            inputTravelExpenseCHFAmount = formatChfAmount travelExpenseCHFAmount
            inputTravelExpenseEvidence = travelExpenseEvidence
         in TravelExpenseInput {..}
      inputTotalTravelExpenses = formatChfAmount taxesReportTotalTravelExpenses
      inputInternetExpenses = flip map taxesReportInternetExpenses $ \InternetExpense {..} ->
        let inputInternetExpenseDay = Timestamp.toDay internetExpenseTimestamp
            inputInternetExpenseDescription = Description.toText internetExpenseDescription
            inputInternetExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount internetExpenseCurrency internetExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol internetExpenseCurrency
                }
            inputInternetExpenseCHFAmount = formatChfAmount internetExpenseCHFAmount
            inputInternetExpenseEvidence = internetExpenseEvidence
         in InternetExpenseInput {..}
      inputTotalInternetExpenses = formatChfAmount taxesReportTotalInternetExpenses
      inputHealthExpenses = flip map taxesReportHealthExpenses $ \HealthExpense {..} ->
        let inputHealthExpenseDay = Timestamp.toDay healthExpenseTimestamp
            inputHealthExpenseDescription = Description.toText healthExpenseDescription
            inputHealthExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount healthExpenseCurrency healthExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol healthExpenseCurrency
                }
            inputHealthExpenseCHFAmount = formatChfAmount healthExpenseCHFAmount
            inputHealthExpenseEvidence = healthExpenseEvidence
         in HealthExpenseInput {..}
      inputTotalHealthExpenses = formatChfAmount taxesReportTotalHealthExpenses
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
    inputThirdPillarContributions :: ![ThirdPillarContributionInput],
    inputTotalThirdPillarContributions :: !FormattedAmount,
    inputHomeofficeExpenses :: ![HomeofficeExpenseInput],
    inputTotalHomeofficeExpenses :: !FormattedAmount,
    inputElectricityExpenses :: ![ElectricityExpenseInput],
    inputTotalElectricityExpenses :: !FormattedAmount,
    inputPhoneExpenses :: ![PhoneExpenseInput],
    inputTotalPhoneExpenses :: !FormattedAmount,
    inputTravelExpenses :: ![TravelExpenseInput],
    inputTotalTravelExpenses :: !FormattedAmount,
    inputInternetExpenses :: ![InternetExpenseInput],
    inputTotalInternetExpenses :: !FormattedAmount,
    inputHealthExpenses :: ![HealthExpenseInput],
    inputTotalHealthExpenses :: !FormattedAmount
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
        <*> requiredField "third_pillar_contributions" "third pillar contributions"
          .= inputThirdPillarContributions
        <*> requiredField "total_third_pillar_contributions" "total third pillar contributions"
          .= inputTotalThirdPillarContributions
        <*> requiredField "homeoffice_expenses" "homeoffice expenses"
          .= inputHomeofficeExpenses
        <*> requiredField "total_homeoffice_expenses" "total homeoffice expenses"
          .= inputTotalHomeofficeExpenses
        <*> requiredField "electricity_expenses" "electricity expenses"
          .= inputElectricityExpenses
        <*> requiredField "total_electricity_expenses" "total electricity expenses"
          .= inputTotalElectricityExpenses
        <*> requiredField "phone_expenses" "phone expenses"
          .= inputPhoneExpenses
        <*> requiredField "total_phone_expenses" "total phone expenses"
          .= inputTotalPhoneExpenses
        <*> requiredField "travel_expenses" "travel expenses"
          .= inputTravelExpenses
        <*> requiredField "total_travel_expenses" "total travel expenses"
          .= inputTotalTravelExpenses
        <*> requiredField "internet_expenses" "internet expenses"
          .= inputInternetExpenses
        <*> requiredField "total_internet_expenses" "total internet expenses"
          .= inputTotalInternetExpenses
        <*> requiredField "health_expenses" "health insurance expenses"
          .= inputHealthExpenses
        <*> requiredField "total_health_expenses" "total health insurance expenses"
          .= inputTotalHealthExpenses

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

data ThirdPillarContributionInput = ThirdPillarContributionInput
  { inputThirdPillarContributionDay :: !Day,
    inputThirdPillarContributionDescription :: !Text,
    inputThirdPillarContributionCHFAmount :: !FormattedAmount,
    inputThirdPillarContributionEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec ThirdPillarContributionInput where
  codec =
    object "ThirdPillarContributionInput" $
      ThirdPillarContributionInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputThirdPillarContributionDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputThirdPillarContributionDescription
        <*> requiredField "amount_chf" "amount in chf"
          .= inputThirdPillarContributionCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputThirdPillarContributionEvidence

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

data ElectricityExpenseInput = ElectricityExpenseInput
  { inputElectricityExpenseDay :: !Day,
    inputElectricityExpenseDescription :: !Text,
    inputElectricityExpenseAmount :: !AmountWithCurrency,
    inputElectricityExpenseCHFAmount :: !FormattedAmount,
    inputElectricityExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec ElectricityExpenseInput where
  codec =
    object "ElectricityExpenseInput" $
      ElectricityExpenseInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputElectricityExpenseDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputElectricityExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputElectricityExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputElectricityExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputElectricityExpenseEvidence

data PhoneExpenseInput = PhoneExpenseInput
  { inputPhoneExpenseDay :: !Day,
    inputPhoneExpenseDescription :: !Text,
    inputPhoneExpenseAmount :: !AmountWithCurrency,
    inputPhoneExpenseCHFAmount :: !FormattedAmount,
    inputPhoneExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec PhoneExpenseInput where
  codec =
    object "PhoneExpenseInput" $
      PhoneExpenseInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputPhoneExpenseDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputPhoneExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputPhoneExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputPhoneExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputPhoneExpenseEvidence

data TravelExpenseInput = TravelExpenseInput
  { inputTravelExpenseDay :: !Day,
    inputTravelExpenseDescription :: !Text,
    inputTravelExpenseAmount :: !AmountWithCurrency,
    inputTravelExpenseCHFAmount :: !FormattedAmount,
    inputTravelExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec TravelExpenseInput where
  codec =
    object "TravelExpenseInput" $
      TravelExpenseInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputTravelExpenseDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputTravelExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputTravelExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputTravelExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputTravelExpenseEvidence

data InternetExpenseInput = InternetExpenseInput
  { inputInternetExpenseDay :: !Day,
    inputInternetExpenseDescription :: !Text,
    inputInternetExpenseAmount :: !AmountWithCurrency,
    inputInternetExpenseCHFAmount :: !FormattedAmount,
    inputInternetExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec InternetExpenseInput where
  codec =
    object "InternetExpenseInput" $
      InternetExpenseInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputInternetExpenseDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputInternetExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputInternetExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputInternetExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputInternetExpenseEvidence

data HealthExpenseInput = HealthExpenseInput
  { inputHealthExpenseDay :: !Day,
    inputHealthExpenseDescription :: !Text,
    inputHealthExpenseAmount :: !AmountWithCurrency,
    inputHealthExpenseCHFAmount :: !FormattedAmount,
    inputHealthExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec HealthExpenseInput where
  codec =
    object "HealthExpenseInput" $
      HealthExpenseInput
        <$> requiredField "day" "day of homeofficeexpense"
          .= inputHealthExpenseDay
        <*> requiredField "description" "description of homeofficeexpense"
          .= inputHealthExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputHealthExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputHealthExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputHealthExpenseEvidence

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
