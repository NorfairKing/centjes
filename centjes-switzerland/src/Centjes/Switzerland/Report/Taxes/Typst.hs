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
import Centjes.Switzerland.Report.FormattedAmount
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
  let formatChfAmount = formatAmount taxesReportCHF

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
                        ( \(cur, (amount, chfAmount)) ->
                            ( CurrencySymbol.toText (currencySymbol cur),
                              Balance
                                { balanceOriginal = formatAmount cur amount,
                                  balanceConverted = formatChfAmount chfAmount
                                }
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
                  let daycareExpenseInputDay = Timestamp.toDay daycareExpenseTimestamp
                      daycareExpenseInputDescription = Description.toText daycareExpenseDescription
                      daycareExpenseInputAmount =
                        AmountWithCurrency
                          { amountWithCurrencyAmount = formatAmount daycareExpenseCurrency daycareExpenseAmount,
                            amountWithCurrencyCurrency = currencySymbol daycareExpenseCurrency
                          }
                      daycareExpenseInputCHFAmount = formatChfAmount daycareExpenseCHFAmount
                      daycareExpenseInputEvidence = daycareExpenseEvidence
                   in DaycareExpenseInput {..},
                childrenCostsInputTotalDaycare = formatChfAmount childrenCostsTotalDaycare
              }
      inputTotalRevenues = formatChfAmount taxesReportTotalRevenues
      inputRevenues = flip map taxesReportRevenues $ \Revenue {..} ->
        let revenueInputDay = Timestamp.toDay revenueTimestamp
            revenueInputDescription = Description.toText revenueDescription
            revenueInputAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount revenueCurrency revenueAmount,
                  amountWithCurrencyCurrency = currencySymbol revenueCurrency
                }
            revenueInputCHFAmount = formatChfAmount revenueCHFAmount
            revenueInputEvidence = revenueEvidence
         in RevenueInput {..}
      inputTotalAssets = formatChfAmount taxesReportTotalAssets
      inputThirdPillarContributions = flip map taxesReportThirdPillarContributions $ \ThirdPillarContribution {..} ->
        let thirdPillarContributionInputDay = Timestamp.toDay thirdPillarContributionTimestamp
            thirdPillarContributionInputDescription = Description.toText thirdPillarContributionDescription
            thirdPillarContributionInputCHFAmount = formatChfAmount thirdPillarContributionCHFAmount
            thirdPillarContributionInputEvidence = thirdPillarContributionEvidence
         in ThirdPillarContributionInput {..}
      inputTotalThirdPillarContributions = formatChfAmount taxesReportTotalThirdPillarContributions
      convertPrivateExpense PrivateExpense {..} =
        PrivateExpenseInput
          { privateExpenseInputDay = Timestamp.toDay privateExpenseTimestamp,
            privateExpenseInputDescription = Description.toText privateExpenseDescription,
            privateExpenseInputAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount privateExpenseCurrency privateExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol privateExpenseCurrency
                },
            privateExpenseInputCHFAmount = formatChfAmount privateExpenseCHFAmount,
            privateExpenseInputEvidence = privateExpenseEvidence
          }
      convertPartitionedExpenses convertExpense PartitionedExpenses {..} =
        PartitionedExpensesInput
          { partitionedExpensesInputBusinessExpenses = map convertExpense partitionedExpensesBusinessExpenses,
            partitionedExpensesInputTotalBusinessExpenses = formatChfAmount partitionedExpensesTotalBusinessExpenses,
            partitionedExpensesInputPrivateExpenses = map convertPrivateExpense partitionedExpensesPrivateExpenses,
            partitionedExpensesInputTotalPrivateExpenses = formatChfAmount partitionedExpensesTotalPrivateExpenses,
            partitionedExpensesInputTotalExpenses = maybe (FormattedAmount "ERROR") formatChfAmount $ Amount.add partitionedExpensesTotalBusinessExpenses partitionedExpensesTotalPrivateExpenses
          }
      inputInsuranceExpenses =
        convertPartitionedExpenses
          ( \InsuranceExpense {..} ->
              let insuranceExpenseInputDay = Timestamp.toDay insuranceExpenseTimestamp
                  insuranceExpenseInputDescription = Description.toText insuranceExpenseDescription
                  insuranceExpenseInputAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount insuranceExpenseCurrency insuranceExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol insuranceExpenseCurrency
                      }
                  insuranceExpenseInputCHFAmount = formatChfAmount insuranceExpenseCHFAmount
                  insuranceExpenseInputEvidence = insuranceExpenseEvidence
               in InsuranceExpenseInput {..}
          )
          taxesReportInsuranceExpenses
      inputHomeofficeExpenses =
        convertPartitionedExpenses
          ( \HomeofficeExpense {..} ->
              let homeofficeExpenseInputDay = Timestamp.toDay homeofficeExpenseTimestamp
                  homeofficeExpenseInputDescription = Description.toText homeofficeExpenseDescription
                  homeofficeExpenseInputAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount homeofficeExpenseCurrency homeofficeExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol homeofficeExpenseCurrency
                      }
                  homeofficeExpenseInputCHFAmount = formatChfAmount homeofficeExpenseCHFAmount
                  homeofficeExpenseInputEvidence = homeofficeExpenseEvidence
               in HomeofficeExpenseInput {..}
          )
          taxesReportHomeofficeExpenses
      inputElectricityExpenses =
        convertPartitionedExpenses
          ( \ElectricityExpense {..} ->
              let electricityExpenseInputDay = Timestamp.toDay electricityExpenseTimestamp
                  electricityExpenseInputDescription = Description.toText electricityExpenseDescription
                  electricityExpenseInputAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount electricityExpenseCurrency electricityExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol electricityExpenseCurrency
                      }
                  electricityExpenseInputCHFAmount = formatChfAmount electricityExpenseCHFAmount
                  electricityExpenseInputEvidence = electricityExpenseEvidence
               in ElectricityExpenseInput {..}
          )
          taxesReportElectricityExpenses
      inputPhoneExpenses =
        convertPartitionedExpenses
          ( \PhoneExpense {..} ->
              let phoneExpenseInputDay = Timestamp.toDay phoneExpenseTimestamp
                  phoneExpenseInputDescription = Description.toText phoneExpenseDescription
                  phoneExpenseInputAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount phoneExpenseCurrency phoneExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol phoneExpenseCurrency
                      }
                  phoneExpenseInputCHFAmount = formatChfAmount phoneExpenseCHFAmount
                  phoneExpenseInputEvidence = phoneExpenseEvidence
               in PhoneExpenseInput {..}
          )
          taxesReportPhoneExpenses
      inputTravelExpenses =
        convertPartitionedExpenses
          ( \TravelExpense {..} ->
              let travelExpenseInputDay = Timestamp.toDay travelExpenseTimestamp
                  travelExpenseInputDescription = Description.toText travelExpenseDescription
                  travelExpenseInputAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount travelExpenseCurrency travelExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol travelExpenseCurrency
                      }
                  travelExpenseInputCHFAmount = formatChfAmount travelExpenseCHFAmount
                  travelExpenseInputEvidence = travelExpenseEvidence
               in TravelExpenseInput {..}
          )
          taxesReportTravelExpenses
      inputInternetExpenses =
        convertPartitionedExpenses
          ( \InternetExpense {..} ->
              let internetExpenseInputDay = Timestamp.toDay internetExpenseTimestamp
                  internetExpenseInputDescription = Description.toText internetExpenseDescription
                  internetExpenseInputAmount =
                    AmountWithCurrency
                      { amountWithCurrencyAmount = formatAmount internetExpenseCurrency internetExpenseAmount,
                        amountWithCurrencyCurrency = currencySymbol internetExpenseCurrency
                      }
                  internetExpenseInputCHFAmount = formatChfAmount internetExpenseCHFAmount
                  internetExpenseInputEvidence = internetExpenseEvidence
               in InternetExpenseInput {..}
          )
          taxesReportInternetExpenses
      inputOtherBusinessExpenses =
        flip map taxesReportOtherBusinessExpenses $ \OtherBusinessExpense {..} ->
          let otherBusinessExpenseInputDay = Timestamp.toDay otherBusinessExpenseTimestamp
              otherBusinessExpenseInputDescription = Description.toText otherBusinessExpenseDescription
              otherBusinessExpenseInputAmount =
                AmountWithCurrency
                  { amountWithCurrencyAmount = formatAmount otherBusinessExpenseCurrency otherBusinessExpenseAmount,
                    amountWithCurrencyCurrency = currencySymbol otherBusinessExpenseCurrency
                  }
              otherBusinessExpenseInputCHFAmount = formatChfAmount otherBusinessExpenseCHFAmount
              otherBusinessExpenseInputEvidence = otherBusinessExpenseEvidence
           in OtherBusinessExpenseInput {..}
      inputTotalOtherBusinessExpenses = formatChfAmount taxesReportTotalOtherBusinessExpenses
      convertHealthExpenses = map $ \HealthExpense {..} ->
        let healthExpenseInputDay = Timestamp.toDay healthExpenseTimestamp
            healthExpenseInputDescription = Description.toText healthExpenseDescription
            healthExpenseInputAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount healthExpenseCurrency healthExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol healthExpenseCurrency
                }
            healthExpenseInputCHFAmount = formatChfAmount healthExpenseCHFAmount
            healthExpenseInputEvidence = healthExpenseEvidence
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
        let educationExpenseInputDay = Timestamp.toDay educationExpenseTimestamp
            educationExpenseInputDescription = Description.toText educationExpenseDescription
            educationExpenseInputAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount educationExpenseCurrency educationExpenseAmount,
                  amountWithCurrencyCurrency = currencySymbol educationExpenseCurrency
                }
            educationExpenseInputCHFAmount = formatChfAmount educationExpenseCHFAmount
            educationExpenseInputEvidence = educationExpenseEvidence
         in EducationExpenseInput {..}
      inputTotalEducationExpenses = formatChfAmount taxesReportTotalEducationExpenses
      inputDonations = flip map taxesReportDonations $ \Donation {..} ->
        let donationInputDay = Timestamp.toDay donationTimestamp
            donationInputDescription = Description.toText donationDescription
            donationInputAmount =
              AmountWithCurrency
                { amountWithCurrencyAmount = formatAmount donationCurrency donationAmount,
                  amountWithCurrencyCurrency = currencySymbol donationCurrency
                }
            donationInputCHFAmount = formatChfAmount donationCHFAmount
            donationInputEvidence = donationEvidence
         in DonationInput {..}
      inputTotalDonations = formatChfAmount taxesReportTotalDonations
      inputMovables = convertDepreciationSchedule taxesReportMovables
      inputMachinery = convertDepreciationSchedule taxesReportMachinery
   in Input {..}

-- Note that this is a separate type from the ETax 'XMLReport' because there
-- is more information in the README than there is in the ETax
data Input = Input
  { inputLastName :: !Text,
    inputFirstName :: !Text,
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
    inputOtherBusinessExpenses :: ![OtherBusinessExpenseInput],
    inputTotalOtherBusinessExpenses :: !FormattedAmount,
    inputHealthCosts :: !HealthCostsInput,
    inputEducationExpenses :: ![EducationExpenseInput],
    inputTotalEducationExpenses :: !FormattedAmount,
    inputDonations :: ![DonationInput],
    inputTotalDonations :: !FormattedAmount,
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
        <*> requiredField "other_business_expenses" "other business expenses"
          .= inputOtherBusinessExpenses
        <*> requiredField "total_other_business_expenses" "total other business expenses"
          .= inputTotalOtherBusinessExpenses
        <*> requiredField "health_costs" "health costs"
          .= inputHealthCosts
        <*> requiredField "education_expenses" "education expenses"
          .= inputEducationExpenses
        <*> requiredField "total_education_expenses" "total education expenses"
          .= inputTotalEducationExpenses
        <*> requiredField "donations" "donations"
          .= inputDonations
        <*> requiredField "total_donations" "total donations"
          .= inputTotalDonations
        <*> requiredField "movables" "movables depreciation schedule"
          .= inputMovables
        <*> requiredField "machinery" "machinery depreciation schedule"
          .= inputMachinery

data AssetInput = AssetInput
  { assetInputAccountName :: !AccountName,
    assetInputBalances :: !(Map Text Balance),
    assetInputConvertedBalance :: !FormattedAmount,
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
  { balanceOriginal :: !FormattedAmount,
    balanceConverted :: !FormattedAmount
  }

instance HasCodec Balance where
  codec =
    object "Balance" $
      Balance
        <$> requiredField "original" "balance in original currency" .= balanceOriginal
        <*> requiredField "converted" "balance in CHF" .= balanceConverted

data RevenueInput = RevenueInput
  { revenueInputDay :: !Day,
    revenueInputDescription :: !Text,
    revenueInputAmount :: !AmountWithCurrency,
    revenueInputCHFAmount :: !FormattedAmount,
    revenueInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec RevenueInput where
  codec =
    object "RevenueInput" $
      RevenueInput
        <$> requiredField "day" "day of revenue"
          .= revenueInputDay
        <*> requiredField "description" "description of revenue"
          .= revenueInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= revenueInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= revenueInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= revenueInputEvidence

data ThirdPillarContributionInput = ThirdPillarContributionInput
  { thirdPillarContributionInputDay :: !Day,
    thirdPillarContributionInputDescription :: !Text,
    thirdPillarContributionInputCHFAmount :: !FormattedAmount,
    thirdPillarContributionInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec ThirdPillarContributionInput where
  codec =
    object "ThirdPillarContributionInput" $
      ThirdPillarContributionInput
        <$> requiredField "day" "day of contribution"
          .= thirdPillarContributionInputDay
        <*> requiredField "description" "description of contribution"
          .= thirdPillarContributionInputDescription
        <*> requiredField "amount_chf" "amount in chf"
          .= thirdPillarContributionInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= thirdPillarContributionInputEvidence

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
  { privateExpenseInputDay :: !Day,
    privateExpenseInputDescription :: !Text,
    privateExpenseInputAmount :: !AmountWithCurrency,
    privateExpenseInputCHFAmount :: !FormattedAmount,
    privateExpenseInputEvidence :: ![Path Rel File]
  }

instance HasCodec PrivateExpenseInput where
  codec =
    object "PrivateExpenseInput" $
      PrivateExpenseInput
        <$> requiredField "day" "day of expense"
          .= privateExpenseInputDay
        <*> requiredField "description" "description of expense"
          .= privateExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= privateExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= privateExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= privateExpenseInputEvidence

data InsuranceExpenseInput = InsuranceExpenseInput
  { insuranceExpenseInputDay :: !Day,
    insuranceExpenseInputDescription :: !Text,
    insuranceExpenseInputAmount :: !AmountWithCurrency,
    insuranceExpenseInputCHFAmount :: !FormattedAmount,
    insuranceExpenseInputEvidence :: ![Path Rel File]
  }

instance HasCodec InsuranceExpenseInput where
  codec =
    object "InsuranceExpenseInput" $
      InsuranceExpenseInput
        <$> requiredField "day" "day of insurance expense"
          .= insuranceExpenseInputDay
        <*> requiredField "description" "description of insurance expense"
          .= insuranceExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= insuranceExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= insuranceExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= insuranceExpenseInputEvidence

data HomeofficeExpenseInput = HomeofficeExpenseInput
  { homeofficeExpenseInputDay :: !Day,
    homeofficeExpenseInputDescription :: !Text,
    homeofficeExpenseInputAmount :: !AmountWithCurrency,
    homeofficeExpenseInputCHFAmount :: !FormattedAmount,
    homeofficeExpenseInputEvidence :: ![Path Rel File]
  }

instance HasCodec HomeofficeExpenseInput where
  codec =
    object "HomeofficeExpenseInput" $
      HomeofficeExpenseInput
        <$> requiredField "day" "day of homeoffice expense"
          .= homeofficeExpenseInputDay
        <*> requiredField "description" "description of homeoffice expense"
          .= homeofficeExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= homeofficeExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= homeofficeExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= homeofficeExpenseInputEvidence

data ElectricityExpenseInput = ElectricityExpenseInput
  { electricityExpenseInputDay :: !Day,
    electricityExpenseInputDescription :: !Text,
    electricityExpenseInputAmount :: !AmountWithCurrency,
    electricityExpenseInputCHFAmount :: !FormattedAmount,
    electricityExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec ElectricityExpenseInput where
  codec =
    object "ElectricityExpenseInput" $
      ElectricityExpenseInput
        <$> requiredField "day" "day of electricity expense"
          .= electricityExpenseInputDay
        <*> requiredField "description" "description of electricity expense"
          .= electricityExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= electricityExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= electricityExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= electricityExpenseInputEvidence

data PhoneExpenseInput = PhoneExpenseInput
  { phoneExpenseInputDay :: !Day,
    phoneExpenseInputDescription :: !Text,
    phoneExpenseInputAmount :: !AmountWithCurrency,
    phoneExpenseInputCHFAmount :: !FormattedAmount,
    phoneExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec PhoneExpenseInput where
  codec =
    object "PhoneExpenseInput" $
      PhoneExpenseInput
        <$> requiredField "day" "day of phone expense"
          .= phoneExpenseInputDay
        <*> requiredField "description" "description of phone expense"
          .= phoneExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= phoneExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= phoneExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= phoneExpenseInputEvidence

data TravelExpenseInput = TravelExpenseInput
  { travelExpenseInputDay :: !Day,
    travelExpenseInputDescription :: !Text,
    travelExpenseInputAmount :: !AmountWithCurrency,
    travelExpenseInputCHFAmount :: !FormattedAmount,
    travelExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec TravelExpenseInput where
  codec =
    object "TravelExpenseInput" $
      TravelExpenseInput
        <$> requiredField "day" "day of travel expense"
          .= travelExpenseInputDay
        <*> requiredField "description" "description of travel expense"
          .= travelExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= travelExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= travelExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= travelExpenseInputEvidence

data InternetExpenseInput = InternetExpenseInput
  { internetExpenseInputDay :: !Day,
    internetExpenseInputDescription :: !Text,
    internetExpenseInputAmount :: !AmountWithCurrency,
    internetExpenseInputCHFAmount :: !FormattedAmount,
    internetExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec InternetExpenseInput where
  codec =
    object "InternetExpenseInput" $
      InternetExpenseInput
        <$> requiredField "day" "day of internet expense"
          .= internetExpenseInputDay
        <*> requiredField "description" "description of internet expense"
          .= internetExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= internetExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= internetExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= internetExpenseInputEvidence

data OtherBusinessExpenseInput = OtherBusinessExpenseInput
  { otherBusinessExpenseInputDay :: !Day,
    otherBusinessExpenseInputDescription :: !Text,
    otherBusinessExpenseInputAmount :: !AmountWithCurrency,
    otherBusinessExpenseInputCHFAmount :: !FormattedAmount,
    otherBusinessExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec OtherBusinessExpenseInput where
  codec =
    object "OtherBusinessExpenseInput" $
      OtherBusinessExpenseInput
        <$> requiredField "day" "day of other business expense"
          .= otherBusinessExpenseInputDay
        <*> requiredField "description" "description of other business expense"
          .= otherBusinessExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= otherBusinessExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= otherBusinessExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= otherBusinessExpenseInputEvidence

data HealthExpenseInput = HealthExpenseInput
  { healthExpenseInputDay :: !Day,
    healthExpenseInputDescription :: !Text,
    healthExpenseInputAmount :: !AmountWithCurrency,
    healthExpenseInputCHFAmount :: !FormattedAmount,
    healthExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec HealthExpenseInput where
  codec =
    object "HealthExpenseInput" $
      HealthExpenseInput
        <$> requiredField "day" "day of health expense"
          .= healthExpenseInputDay
        <*> requiredField "description" "description of health expense"
          .= healthExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= healthExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= healthExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= healthExpenseInputEvidence

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
  { daycareExpenseInputDay :: !Day,
    daycareExpenseInputDescription :: !Text,
    daycareExpenseInputAmount :: !AmountWithCurrency,
    daycareExpenseInputCHFAmount :: !FormattedAmount,
    daycareExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec DaycareExpenseInput where
  codec =
    object "DaycareExpenseInput" $
      DaycareExpenseInput
        <$> requiredField "day" "day of daycare expense"
          .= daycareExpenseInputDay
        <*> requiredField "description" "description of daycare expense"
          .= daycareExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= daycareExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= daycareExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= daycareExpenseInputEvidence

data EducationExpenseInput = EducationExpenseInput
  { educationExpenseInputDay :: !Day,
    educationExpenseInputDescription :: !Text,
    educationExpenseInputAmount :: !AmountWithCurrency,
    educationExpenseInputCHFAmount :: !FormattedAmount,
    educationExpenseInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec EducationExpenseInput where
  codec =
    object "EducationExpenseInput" $
      EducationExpenseInput
        <$> requiredField "day" "day of education expense"
          .= educationExpenseInputDay
        <*> requiredField "description" "description of education expense"
          .= educationExpenseInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= educationExpenseInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= educationExpenseInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= educationExpenseInputEvidence

data DonationInput = DonationInput
  { donationInputDay :: !Day,
    donationInputDescription :: !Text,
    donationInputAmount :: !AmountWithCurrency,
    donationInputCHFAmount :: !FormattedAmount,
    donationInputEvidence :: !(NonEmpty (Path Rel File))
  }

instance HasCodec DonationInput where
  codec =
    object "DonationInput" $
      DonationInput
        <$> requiredField "day" "day of donation"
          .= donationInputDay
        <*> requiredField "description" "description of donation"
          .= donationInputDescription
        <*> requiredField "amount" "amount in original currency"
          .= donationInputAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= donationInputCHFAmount
        <*> requiredField "evidence" "evidence"
          .= donationInputEvidence

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
  { amountWithCurrencyAmount :: !FormattedAmount,
    amountWithCurrencyCurrency :: !CurrencySymbol
  }

instance HasCodec AmountWithCurrency where
  codec =
    object "AmountWithCurrency" $
      AmountWithCurrency
        <$> requiredField "formatted" "formatted amount"
          .= amountWithCurrencyAmount
        <*> requiredField "symbol" "currency symbol"
          .= amountWithCurrencyCurrency
