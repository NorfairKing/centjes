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
      inputChildrenCosts =
        let ChildrenCosts {..} = taxesReportChildrenCosts
         in ChildrenCostsInput
              { childrenCostsInputDaycare = flip map childrenCostsDaycare $ \DaycareExpense {..} ->
                  let inputDaycareExpenseDay = Timestamp.toDay daycareExpenseTimestamp
                      inputDaycareExpenseDescription = Description.toText daycareExpenseDescription
                      inputDaycareExpenseAmount =
                        AmountWithCurrency
                          { amountWithCurrencyAmount = formatAmount daycareExpenseCurrency daycareExpenseAmount,
                            amountWithCurrencyCurrency = currencySymbol daycareExpenseCurrency
                          }
                      inputDaycareExpenseCHFAmount = formatChfAmount daycareExpenseCHFAmount
                      inputDaycareExpenseEvidence = daycareExpenseEvidence
                   in DaycareExpenseInput {..},
                childrenCostsInputTotalDaycare = formatChfAmount childrenCostsTotalDaycare
              }
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
      convertPrivateExpense PrivateExpense {..} =
        PrivateExpenseInput
          { inputPrivateExpenseDay = Timestamp.toDay privateExpenseTimestamp,
            inputPrivateExpenseDescription = Description.toText privateExpenseDescription,
            inputPrivateExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount privateExpenseCurrency privateExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol privateExpenseCurrency
                },
            inputPrivateExpenseCHFAmount = formatChfAmount privateExpenseCHFAmount,
            inputPrivateExpenseEvidence = privateExpenseEvidence
          }
      convertPartitionedExpenses convertExpense PartitionedExpenses {..} =
        PartitionedExpensesInput
          { partitionedExpensesInputBusinessExpenses = map convertExpense partitionedExpensesBusinessExpenses,
            partitionedExpensesInputTotalBusinessExpenses = formatChfAmount partitionedExpensesTotalBusinessExpenses,
            partitionedExpensesInputPrivateExpenses = map convertPrivateExpense partitionedExpensesPrivateExpenses,
            partitionedExpensesInputTotalPrivateExpenses = formatChfAmount partitionedExpensesTotalPrivateExpenses,
            partitionedExpensesInputTotalExpenses = maybe "ERROR" formatChfAmount $ Amount.add partitionedExpensesTotalBusinessExpenses partitionedExpensesTotalPrivateExpenses
          }
      inputInsuranceExpenses =
        convertPartitionedExpenses
          ( \InsuranceExpense {..} ->
              let inputInsuranceExpenseDay = Timestamp.toDay insuranceExpenseTimestamp
                  inputInsuranceExpenseDescription = Description.toText insuranceExpenseDescription
                  inputInsuranceExpenseAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount insuranceExpenseCurrency insuranceExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol insuranceExpenseCurrency
                      }
                  inputInsuranceExpenseCHFAmount = formatChfAmount insuranceExpenseCHFAmount
                  inputInsuranceExpenseEvidence = insuranceExpenseEvidence
               in InsuranceExpenseInput {..}
          )
          taxesReportInsuranceExpenses
      inputHomeofficeExpenses =
        convertPartitionedExpenses
          ( \HomeofficeExpense {..} ->
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
          )
          taxesReportHomeofficeExpenses
      inputElectricityExpenses =
        convertPartitionedExpenses
          ( \ElectricityExpense {..} ->
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
          )
          taxesReportElectricityExpenses
      inputPhoneExpenses =
        convertPartitionedExpenses
          ( \PhoneExpense {..} ->
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
          )
          taxesReportPhoneExpenses
      inputTravelExpenses =
        convertPartitionedExpenses
          ( \TravelExpense {..} ->
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
          )
          taxesReportTravelExpenses
      inputInternetExpenses =
        convertPartitionedExpenses
          ( \InternetExpense {..} ->
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
          )
          taxesReportInternetExpenses
      convertHealthExpenses = map $ \HealthExpense {..} ->
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
      inputHealthCosts =
        let HealthCosts {..} = taxesReportHealthCosts
         in HealthCostsInput
              { healthCostsInputInsurancePremiums = convertHealthExpenses healthCostsInsurancePremiums,
                healthCostsInputTotalInsurancePremiums = formatChfAmount healthCostsTotalInsurancePremiums,
                healthCostsInputOther = convertHealthExpenses healthCostsOther,
                healthCostsInputTotalOther = formatChfAmount healthCostsTotalOther,
                healthCostsInputDentist = convertHealthExpenses healthCostsDentist,
                healthCostsInputTotalDentist = formatChfAmount healthCostsTotalDentist,
                healthCostsInputDoctor = convertHealthExpenses healthCostsDoctor,
                healthCostsInputTotalDoctor = formatChfAmount healthCostsTotalDoctor,
                healthCostsInputHospital = convertHealthExpenses healthCostsHospital,
                healthCostsInputTotalHospital = formatChfAmount healthCostsTotalHospital,
                healthCostsInputTherapy = convertHealthExpenses healthCostsTherapy,
                healthCostsInputTotalTherapy = formatChfAmount healthCostsTotalTherapy
              }
      convertDepreciationSchedule DepreciationSchedule {..} =
        DepreciationScheduleInput
          { depreciationScheduleInputDepreciationRate = printf "%.0f%%" $ (realToFrac :: Ratio Natural -> Double) $ depreciationScheduleDepreciationRate * 100,
            depreciationScheduleInputOpeningBalance = formatChfAmount depreciationScheduleOpeningBalance,
            depreciationScheduleInputOpeningBalanceEvidence = depreciationScheduleOpeningBalanceEvidence,
            depreciationScheduleInputPurchases = flip map depreciationSchedulePurchases $ \DepreciationPurchase {..} ->
              DepreciationPurchaseInput
                { depreciationPurchaseInputDay = Timestamp.toDay depreciationPurchaseTimestamp,
                  depreciationPurchaseInputDescription = Description.toText depreciationPurchaseDescription,
                  depreciationPurchaseInputAmount = formatChfAmount depreciationPurchaseAmount,
                  depreciationPurchaseInputEvidence = depreciationPurchaseEvidence
                },
            depreciationScheduleInputTotalPurchases = formatChfAmount depreciationScheduleTotalPurchases,
            depreciationScheduleInputDepreciation = formatChfAmount depreciationScheduleDepreciation,
            depreciationScheduleInputClosingBalance = formatChfAmount depreciationScheduleClosingBalance
          }
      inputEducationExpenses = flip map taxesReportEducationExpenses $ \EducationExpense {..} ->
        let inputEducationExpenseDay = Timestamp.toDay educationExpenseTimestamp
            inputEducationExpenseDescription = Description.toText educationExpenseDescription
            inputEducationExpenseAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount educationExpenseCurrency educationExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol educationExpenseCurrency
                }
            inputEducationExpenseCHFAmount = formatChfAmount educationExpenseCHFAmount
            inputEducationExpenseEvidence = educationExpenseEvidence
         in EducationExpenseInput {..}
      inputTotalEducationExpenses = formatChfAmount taxesReportTotalEducationExpenses
      inputMovables = convertDepreciationSchedule taxesReportMovables
      inputMachinery = convertDepreciationSchedule taxesReportMachinery
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
    inputChildrenCosts :: !ChildrenCostsInput,
    inputRevenues :: ![RevenueInput],
    inputTotalRevenues :: !FormattedAmount,
    inputThirdPillarContributions :: ![ThirdPillarContributionInput],
    inputTotalThirdPillarContributions :: !FormattedAmount,
    inputInsuranceExpenses :: !(PartitionedExpensesInput InsuranceExpenseInput),
    inputHomeofficeExpenses :: !(PartitionedExpensesInput HomeofficeExpenseInput),
    inputElectricityExpenses :: !(PartitionedExpensesInput ElectricityExpenseInput),
    inputPhoneExpenses :: !(PartitionedExpensesInput PhoneExpenseInput),
    inputTravelExpenses :: !(PartitionedExpensesInput TravelExpenseInput),
    inputInternetExpenses :: !(PartitionedExpensesInput InternetExpenseInput),
    inputHealthCosts :: !HealthCostsInput,
    inputEducationExpenses :: ![EducationExpenseInput],
    inputTotalEducationExpenses :: !FormattedAmount,
    inputMovables :: !DepreciationScheduleInput,
    inputMachinery :: !DepreciationScheduleInput
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
        <*> requiredField "children_costs" "children costs"
          .= inputChildrenCosts
        <*> requiredField "revenues" "revenues"
          .= inputRevenues
        <*> requiredField "total_revenues" "total revenues"
          .= inputTotalRevenues
        <*> requiredField "third_pillar_contributions" "third pillar contributions"
          .= inputThirdPillarContributions
        <*> requiredField "total_third_pillar_contributions" "total third pillar contributions"
          .= inputTotalThirdPillarContributions
        <*> requiredField "insurance_expenses" "insurance expenses"
          .= inputInsuranceExpenses
        <*> requiredField "homeoffice_expenses" "homeoffice expenses"
          .= inputHomeofficeExpenses
        <*> requiredField "electricity_expenses" "electricity expenses"
          .= inputElectricityExpenses
        <*> requiredField "phone_expenses" "phone expenses"
          .= inputPhoneExpenses
        <*> requiredField "travel_expenses" "travel expenses"
          .= inputTravelExpenses
        <*> requiredField "internet_expenses" "internet expenses"
          .= inputInternetExpenses
        <*> requiredField "health_costs" "health costs"
          .= inputHealthCosts
        <*> requiredField "education_expenses" "education expenses"
          .= inputEducationExpenses
        <*> requiredField "total_education_expenses" "total education expenses"
          .= inputTotalEducationExpenses
        <*> requiredField "movables" "movables depreciation schedule"
          .= inputMovables
        <*> requiredField "machinery" "machinery depreciation schedule"
          .= inputMachinery

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
        <$> requiredField "day" "day of contribution"
          .= inputThirdPillarContributionDay
        <*> requiredField "description" "description of contribution"
          .= inputThirdPillarContributionDescription
        <*> requiredField "amount_chf" "amount in chf"
          .= inputThirdPillarContributionCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputThirdPillarContributionEvidence

data PartitionedExpensesInput a = PartitionedExpensesInput
  { partitionedExpensesInputBusinessExpenses :: ![a],
    partitionedExpensesInputTotalBusinessExpenses :: !FormattedAmount,
    partitionedExpensesInputPrivateExpenses :: ![PrivateExpenseInput],
    partitionedExpensesInputTotalPrivateExpenses :: !FormattedAmount,
    partitionedExpensesInputTotalExpenses :: !FormattedAmount
  }

instance (HasCodec a) => HasCodec (PartitionedExpensesInput a) where
  codec =
    object "PartitionedExpensesInput" $
      PartitionedExpensesInput
        <$> requiredField "business_expenses" "business expenses"
          .= partitionedExpensesInputBusinessExpenses
        <*> requiredField "total_business_expenses" "total business expenses"
          .= partitionedExpensesInputTotalBusinessExpenses
        <*> requiredField "private_expenses" "private expenses"
          .= partitionedExpensesInputPrivateExpenses
        <*> requiredField "total_private_expenses" "total private expenses"
          .= partitionedExpensesInputTotalPrivateExpenses
        <*> requiredField "total_expenses" "total expenses (business + private)"
          .= partitionedExpensesInputTotalExpenses

data PrivateExpenseInput = PrivateExpenseInput
  { inputPrivateExpenseDay :: !Day,
    inputPrivateExpenseDescription :: !Text,
    inputPrivateExpenseAmount :: !AmountWithCurrency,
    inputPrivateExpenseCHFAmount :: !FormattedAmount,
    inputPrivateExpenseEvidence :: ![Path Rel File]
  }

instance HasCodec PrivateExpenseInput where
  codec =
    object "PrivateExpenseInput" $
      PrivateExpenseInput
        <$> requiredField "day" "day of expense"
          .= inputPrivateExpenseDay
        <*> requiredField "description" "description of expense"
          .= inputPrivateExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputPrivateExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputPrivateExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputPrivateExpenseEvidence

data InsuranceExpenseInput = InsuranceExpenseInput
  { inputInsuranceExpenseDay :: !Day,
    inputInsuranceExpenseDescription :: !Text,
    inputInsuranceExpenseAmount :: !AmountWithCurrency,
    inputInsuranceExpenseCHFAmount :: !FormattedAmount,
    inputInsuranceExpenseEvidence :: ![Path Rel File]
  }

instance HasCodec InsuranceExpenseInput where
  codec =
    object "InsuranceExpenseInput" $
      InsuranceExpenseInput
        <$> requiredField "day" "day of insurance expense"
          .= inputInsuranceExpenseDay
        <*> requiredField "description" "description of insurance expense"
          .= inputInsuranceExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputInsuranceExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputInsuranceExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputInsuranceExpenseEvidence

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
        <$> requiredField "day" "day of homeoffice expense"
          .= inputHomeofficeExpenseDay
        <*> requiredField "description" "description of homeoffice expense"
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
        <$> requiredField "day" "day of electricity expense"
          .= inputElectricityExpenseDay
        <*> requiredField "description" "description of electricity expense"
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
        <$> requiredField "day" "day of phone expense"
          .= inputPhoneExpenseDay
        <*> requiredField "description" "description of phone expense"
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
        <$> requiredField "day" "day of travel expense"
          .= inputTravelExpenseDay
        <*> requiredField "description" "description of travel expense"
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
        <$> requiredField "day" "day of internet expense"
          .= inputInternetExpenseDay
        <*> requiredField "description" "description of internet expense"
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
        <$> requiredField "day" "day of health expense"
          .= inputHealthExpenseDay
        <*> requiredField "description" "description of health expense"
          .= inputHealthExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputHealthExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputHealthExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputHealthExpenseEvidence

data HealthCostsInput = HealthCostsInput
  { healthCostsInputInsurancePremiums :: ![HealthExpenseInput],
    healthCostsInputTotalInsurancePremiums :: !FormattedAmount,
    healthCostsInputOther :: ![HealthExpenseInput],
    healthCostsInputTotalOther :: !FormattedAmount,
    healthCostsInputDentist :: ![HealthExpenseInput],
    healthCostsInputTotalDentist :: !FormattedAmount,
    healthCostsInputDoctor :: ![HealthExpenseInput],
    healthCostsInputTotalDoctor :: !FormattedAmount,
    healthCostsInputHospital :: ![HealthExpenseInput],
    healthCostsInputTotalHospital :: !FormattedAmount,
    healthCostsInputTherapy :: ![HealthExpenseInput],
    healthCostsInputTotalTherapy :: !FormattedAmount
  }

instance HasCodec HealthCostsInput where
  codec =
    object "HealthCostsInput" $
      HealthCostsInput
        <$> requiredField "insurance_premiums" "health insurance premiums"
          .= healthCostsInputInsurancePremiums
        <*> requiredField "total_insurance_premiums" "total health insurance premiums"
          .= healthCostsInputTotalInsurancePremiums
        <*> requiredField "other" "other health expenses"
          .= healthCostsInputOther
        <*> requiredField "total_other" "total other health expenses"
          .= healthCostsInputTotalOther
        <*> requiredField "dentist" "dentist expenses"
          .= healthCostsInputDentist
        <*> requiredField "total_dentist" "total dentist expenses"
          .= healthCostsInputTotalDentist
        <*> requiredField "doctor" "doctor and prescription expenses"
          .= healthCostsInputDoctor
        <*> requiredField "total_doctor" "total doctor expenses"
          .= healthCostsInputTotalDoctor
        <*> requiredField "hospital" "hospital stay expenses"
          .= healthCostsInputHospital
        <*> requiredField "total_hospital" "total hospital expenses"
          .= healthCostsInputTotalHospital
        <*> requiredField "therapy" "therapy and cure expenses"
          .= healthCostsInputTherapy
        <*> requiredField "total_therapy" "total therapy expenses"
          .= healthCostsInputTotalTherapy

data ChildrenCostsInput = ChildrenCostsInput
  { childrenCostsInputDaycare :: ![DaycareExpenseInput],
    childrenCostsInputTotalDaycare :: !FormattedAmount
  }

instance HasCodec ChildrenCostsInput where
  codec =
    object "ChildrenCostsInput" $
      ChildrenCostsInput
        <$> requiredField "daycare" "daycare expenses"
          .= childrenCostsInputDaycare
        <*> requiredField "total_daycare" "total daycare expenses"
          .= childrenCostsInputTotalDaycare

data DaycareExpenseInput = DaycareExpenseInput
  { inputDaycareExpenseDay :: !Day,
    inputDaycareExpenseDescription :: !Text,
    inputDaycareExpenseAmount :: !AmountWithCurrency,
    inputDaycareExpenseCHFAmount :: !FormattedAmount,
    inputDaycareExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec DaycareExpenseInput where
  codec =
    object "DaycareExpenseInput" $
      DaycareExpenseInput
        <$> requiredField "day" "day of daycare expense"
          .= inputDaycareExpenseDay
        <*> requiredField "description" "description of daycare expense"
          .= inputDaycareExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputDaycareExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputDaycareExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputDaycareExpenseEvidence

data EducationExpenseInput = EducationExpenseInput
  { inputEducationExpenseDay :: !Day,
    inputEducationExpenseDescription :: !Text,
    inputEducationExpenseAmount :: !AmountWithCurrency,
    inputEducationExpenseCHFAmount :: !FormattedAmount,
    inputEducationExpenseEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec EducationExpenseInput where
  codec =
    object "EducationExpenseInput" $
      EducationExpenseInput
        <$> requiredField "day" "day of education expense"
          .= inputEducationExpenseDay
        <*> requiredField "description" "description of education expense"
          .= inputEducationExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputEducationExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputEducationExpenseCHFAmount
        <*> requiredField "evidence" "evidence"
          .= inputEducationExpenseEvidence

data DepreciationScheduleInput = DepreciationScheduleInput
  { depreciationScheduleInputDepreciationRate :: !String,
    depreciationScheduleInputOpeningBalance :: !FormattedAmount,
    depreciationScheduleInputOpeningBalanceEvidence :: !(NonEmpty (Path Rel File)),
    depreciationScheduleInputPurchases :: ![DepreciationPurchaseInput],
    depreciationScheduleInputTotalPurchases :: !FormattedAmount,
    depreciationScheduleInputDepreciation :: !FormattedAmount,
    depreciationScheduleInputClosingBalance :: !FormattedAmount
  }

instance HasCodec DepreciationScheduleInput where
  codec =
    object "DepreciationScheduleInput" $
      DepreciationScheduleInput
        <$> requiredField "depreciation_rate" "depreciation rate"
          .= depreciationScheduleInputDepreciationRate
        <*> requiredField "opening_balance" "opening balance"
          .= depreciationScheduleInputOpeningBalance
        <*> requiredField "opening_balance_evidence" "opening balance evidence"
          .= depreciationScheduleInputOpeningBalanceEvidence
        <*> requiredField "purchases" "purchases"
          .= depreciationScheduleInputPurchases
        <*> requiredField "total_purchases" "total purchases"
          .= depreciationScheduleInputTotalPurchases
        <*> requiredField "depreciation" "depreciation"
          .= depreciationScheduleInputDepreciation
        <*> requiredField "closing_balance" "closing balance"
          .= depreciationScheduleInputClosingBalance

data DepreciationPurchaseInput = DepreciationPurchaseInput
  { depreciationPurchaseInputDay :: !Day,
    depreciationPurchaseInputDescription :: !Text,
    depreciationPurchaseInputAmount :: !FormattedAmount,
    depreciationPurchaseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec DepreciationPurchaseInput where
  codec =
    object "DepreciationPurchaseInput" $
      DepreciationPurchaseInput
        <$> requiredField "day" "day of purchase"
          .= depreciationPurchaseInputDay
        <*> requiredField "description" "description of purchase"
          .= depreciationPurchaseInputDescription
        <*> requiredField "amount_chf" "amount in CHF"
          .= depreciationPurchaseInputAmount
        <*> requiredField "evidence" "evidence"
          .= depreciationPurchaseInputEvidence

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
