{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Types
  ( TaxesInput (..),
    PartitionedExpenseAccounts (..),
    TaxesReport (..),
    PartitionedExpenses (..),
    PrivateExpense (..),
    AssetAccount (..),
    Revenue (..),
    ThirdPillarContribution (..),
    InsuranceExpense (..),
    HomeofficeExpense (..),
    ElectricityExpense (..),
    PhoneExpense (..),
    TravelExpense (..),
    InternetExpense (..),
    HealthExpense (..),
    DepreciationSchedule (..),
    DepreciationPurchase (..),
    TaxesError (..),
  )
where

import Autodocodec
import qualified Centjes.AccountName as AccountName
import Centjes.Convert
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format
import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Module
import qualified Centjes.Module as Syntax
import Centjes.Report.Balance
import Centjes.Report.EvaluatedLedger
import qualified Centjes.Tag as Tag
import Centjes.Validation
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import qualified Money.ConversionRate as Money (ConversionRate)
import Money.QuantisationFactor as Money (QuantisationFactor (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Numeric.Natural
import OptEnvConf
import Path
import Text.Read (readMaybe)

-- | The settings we need to produce a 'TaxesReport'
data TaxesInput = TaxesInput
  { taxesInputLastName :: !Text,
    taxesInputFirstName :: !Text,
    taxesInputInsuredPersonNumber :: !Text,
    taxesInputYear :: !Year,
    taxesInputTagUndeclared :: !Tag,
    taxesInputTagDeductible :: !Tag,
    taxesInputTagNotDeductible :: !Tag,
    taxesInputTagTaxDeductible :: !Tag,
    taxesInputTagNotTaxDeductible :: !Tag,
    taxesInputThirdPillarAssetsAccount :: !AccountName,
    taxesInputThirdPillarInsuranceExpensesAccount :: !AccountName,
    taxesInputHomeofficeExpenseAccounts :: !PartitionedExpenseAccounts,
    taxesInputAccidentInsuranceExpenseAccount :: !AccountName,
    taxesInputDailyAllowanceInsuranceExpenseAccount :: !AccountName,
    taxesInputElectricityExpenseAccounts :: !PartitionedExpenseAccounts,
    taxesInputPhoneExpenseAccounts :: !PartitionedExpenseAccounts,
    taxesInputTravelExpenseAccounts :: !PartitionedExpenseAccounts,
    taxesInputInternetExpenseAccounts :: !PartitionedExpenseAccounts,
    taxesInputHealthExpensesAccount :: !AccountName,
    taxesInputMovablesAssetsAccount :: !AccountName,
    taxesInputMovablesExpensesAccount :: !AccountName,
    taxesInputMovablesDepreciationRate :: !(Ratio Natural),
    taxesInputMachineryAssetsAccount :: !AccountName,
    taxesInputMachineryExpensesAccount :: !AccountName,
    taxesInputMachineryDepreciationRate :: !(Ratio Natural)
  }
  deriving (Show, Generic)

instance Validity TaxesInput

instance HasParser TaxesInput where
  settingsParser = parseTaxesInput

{-# ANN parseTaxesInput ("NOCOVER" :: String) #-}
parseTaxesInput :: Parser TaxesInput
parseTaxesInput = do
  taxesInputLastName <-
    setting
      [ help "your first name",
        conf "first-name"
      ]
  taxesInputFirstName <-
    setting
      [ help "your last name",
        conf "last-name"
      ]
  taxesInputInsuredPersonNumber <-
    setting
      [ help "The AHV identifier.",
        conf "ahv-id",
        example "7461111222233"
      ]
  taxesInputYear <-
    choice
      [ setting
          [ help "the year to produce the report for",
            option,
            reader $ maybeReader readMaybe,
            long "year",
            conf "year",
            metavar "YYYY"
          ],
        runIO $ (\d -> let (y, _, _) = toGregorian d in y) . utctDay <$> getCurrentTime
      ]
  taxesInputTagUndeclared <-
    setting
      [ help "tag to use for undeclared asset accounts",
        reader $ eitherReader Tag.fromString,
        conf "tag-undeclared",
        value "undeclared"
      ]
  taxesInputTagDeductible <-
    setting
      [ help "tag to use for deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-deductible",
        value "deductible"
      ]
  taxesInputTagNotDeductible <-
    setting
      [ help "tag to use for non-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-deductible",
        value "not-deductible"
      ]
  taxesInputTagTaxDeductible <-
    setting
      [ help "tag to use for tax-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-tax-deductible",
        value "tax-deductible"
      ]
  taxesInputTagNotTaxDeductible <-
    setting
      [ help "tag to use for non-tax-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-tax-deductible",
        value "not-tax-deductible"
      ]
  taxesInputThirdPillarAssetsAccount <-
    setting
      [ help "the account to use for third pillar assets",
        reader $ maybeReader AccountName.fromString,
        conf "third-pillar-assets-account",
        value "assets:third-pillar"
      ]
  taxesInputThirdPillarInsuranceExpensesAccount <-
    setting
      [ help "the account to use for third pillar insurance expenses",
        reader $ maybeReader AccountName.fromString,
        conf "third-pillar-insurance-expenses-account",
        value "expenses:insurance:third-pillar"
      ]
  taxesInputAccidentInsuranceExpenseAccount <-
    setting
      [ help "the account to use for accident insurance expenses",
        reader $ maybeReader AccountName.fromString,
        conf "accident-insurance-expenses-account",
        value "expenses:insurance:accident"
      ]
  taxesInputDailyAllowanceInsuranceExpenseAccount <-
    setting
      [ help "the account to use for daily allowance insurance expenses",
        reader $ maybeReader AccountName.fromString,
        conf "daily-allowance-insurance-expenses-account",
        value "expenses:insurance:daily-allowance"
      ]
  taxesInputHomeofficeExpenseAccounts <-
    parsePartitionedExpenseAccounts
      "homeoffice"
      "homeoffice-expenses-account"
      "expenses:rent:homeoffice"
      "homeoffice-private-expenses-account"
      "expenses:rent:private"
  taxesInputElectricityExpenseAccounts <-
    parsePartitionedExpenseAccounts
      "electricity"
      "electricity-expenses-account"
      "expenses:electricity:homeoffice"
      "electricity-private-expenses-account"
      "expenses:electricity:private"
  taxesInputPhoneExpenseAccounts <-
    parsePartitionedExpenseAccounts
      "phone"
      "phone-expenses-account"
      "expenses:phone:professional"
      "phone-private-expenses-account"
      "expenses:phone:private"
  taxesInputTravelExpenseAccounts <-
    parsePartitionedExpenseAccounts
      "travel"
      "travel-expenses-account"
      "expenses:travel:professional"
      "travel-private-expenses-account"
      "expenses:travel:private"
  taxesInputInternetExpenseAccounts <-
    parsePartitionedExpenseAccounts
      "internet"
      "internet-expenses-account"
      "expenses:internet:homeoffice"
      "internet-private-expenses-account"
      "expenses:internet:private"
  taxesInputHealthExpensesAccount <-
    setting
      [ help "the account to use for health insurance expenses",
        reader $ maybeReader AccountName.fromString,
        conf "health-insurance-expenses-account",
        value "expenses:health"
      ]
  taxesInputMovablesAssetsAccount <-
    setting
      [ help "the account to use for movables assets",
        reader $ maybeReader AccountName.fromString,
        conf "movables-assets-account",
        value "assets:movables"
      ]
  taxesInputMovablesExpensesAccount <-
    setting
      [ help "the expenses account for movables purchases",
        reader $ maybeReader AccountName.fromString,
        conf "movables-expenses-account",
        value "expenses:movables"
      ]
  -- Declining-balance depreciation rate for Mobilien (movable property).
  -- Default 25% per ESTV Merkblatt A/1995 Geschäftliche Betriebe:
  -- https://www.estv.admin.ch/dam/estv/de/dokumente/dbst/merkblaetter/dbst-mb-a-1995-geschbetriebe-de.pdf
  taxesInputMovablesDepreciationRate <-
    setting
      [ help "the depreciation rate for movables (e.g. 0.25 for 25%)",
        reader $ eitherReader parseDepreciationRate,
        confWith "movables-depreciation-rate" depreciationRateCodec,
        value (25 % 100)
      ]
  taxesInputMachineryAssetsAccount <-
    setting
      [ help "the account to use for machinery assets",
        reader $ maybeReader AccountName.fromString,
        conf "machinery-assets-account",
        value "assets:machinery"
      ]
  taxesInputMachineryExpensesAccount <-
    setting
      [ help "the expenses account for machinery purchases",
        reader $ maybeReader AccountName.fromString,
        conf "machinery-expenses-account",
        value "expenses:machinery"
      ]
  -- Declining-balance depreciation rate for Maschinen (machinery).
  -- Default 40% per ESTV Merkblatt A/1995 Geschäftliche Betriebe:
  -- https://www.estv.admin.ch/dam/estv/de/dokumente/dbst/merkblaetter/dbst-mb-a-1995-geschbetriebe-de.pdf
  taxesInputMachineryDepreciationRate <-
    setting
      [ help "the depreciation rate for machinery (e.g. 0.4 for 40%)",
        reader $ eitherReader parseDepreciationRate,
        confWith "machinery-depreciation-rate" depreciationRateCodec,
        value (40 % 100)
      ]
  pure TaxesInput {..}

{-# ANN parsePartitionedExpenseAccounts ("NOCOVER" :: String) #-}
parsePartitionedExpenseAccounts ::
  String ->
  String ->
  AccountName ->
  String ->
  AccountName ->
  Parser PartitionedExpenseAccounts
parsePartitionedExpenseAccounts category businessKey businessDefault privateKey privateDefault = do
  partitionedExpenseAccountsBusiness <-
    setting
      [ help $ "the account to use for " <> category <> " expenses",
        reader $ maybeReader AccountName.fromString,
        conf businessKey,
        value businessDefault
      ]
  partitionedExpenseAccountsPrivate <-
    setting
      [ help $ "the account to use for private " <> category <> " expenses",
        reader $ maybeReader AccountName.fromString,
        conf privateKey,
        value privateDefault
      ]
  pure PartitionedExpenseAccounts {..}

parseDepreciationRate :: String -> Either String (Ratio Natural)
parseDepreciationRate s = case DecimalLiteral.fromString s of
  Nothing -> Left $ "Could not parse depreciation rate: " <> s
  Just dl -> case DecimalLiteral.toRatio dl of
    Nothing -> Left $ "Could not convert depreciation rate to ratio: " <> s
    Just r -> Right r

depreciationRateCodec :: JSONCodec (Ratio Natural)
depreciationRateCodec =
  bimapCodec
    parseDepreciationRate
    ( \r -> case DecimalLiteral.fromRatio r of
        Nothing -> show (numerator r) <> "/" <> show (denominator r)
        Just dl -> DecimalLiteral.toString dl
    )
    codec

data PartitionedExpenseAccounts = PartitionedExpenseAccounts
  { partitionedExpenseAccountsBusiness :: !AccountName,
    partitionedExpenseAccountsPrivate :: !AccountName
  }
  deriving (Show, Generic)

instance Validity PartitionedExpenseAccounts

-- | The information we need to produce Taxes reports like the pdfs, zip files,
-- or xml files.
data TaxesReport ann = TaxesReport
  { taxesReportLastName :: !Text,
    taxesReportFirstName :: !Text,
    taxesReportYear :: !Year,
    taxesReportInsuredPersonNumber :: !Text,
    taxesReportCHF :: !(Currency ann),
    taxesReportConversionRates :: !(Map (Currency ann) Money.ConversionRate),
    taxesReportAssetAccounts :: ![AssetAccount ann],
    taxesReportTotalAssets :: !Money.Amount,
    taxesReportRevenues :: ![Revenue ann],
    taxesReportTotalRevenues :: !Money.Amount,
    taxesReportThirdPillarContributions :: ![ThirdPillarContribution ann],
    taxesReportTotalThirdPillarContributions :: !Money.Amount,
    taxesReportInsuranceExpenses :: !(PartitionedExpenses (InsuranceExpense ann) ann),
    taxesReportHomeofficeExpenses :: !(PartitionedExpenses (HomeofficeExpense ann) ann),
    taxesReportElectricityExpenses :: !(PartitionedExpenses (ElectricityExpense ann) ann),
    taxesReportPhoneExpenses :: !(PartitionedExpenses (PhoneExpense ann) ann),
    taxesReportTravelExpenses :: !(PartitionedExpenses (TravelExpense ann) ann),
    taxesReportInternetExpenses :: !(PartitionedExpenses (InternetExpense ann) ann),
    taxesReportHealthExpenses :: !(PartitionedExpenses (HealthExpense ann) ann),
    taxesReportMovables :: !(DepreciationSchedule ann),
    taxesReportMachinery :: !(DepreciationSchedule ann)
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TaxesReport ann) where
  validate tr@TaxesReport {..} =
    mconcat
      [ genericValidate tr,
        declare "the assets sum to the total assets" $
          Amount.sum (map assetAccountConvertedBalance taxesReportAssetAccounts) == Just taxesReportTotalAssets,
        declare "the revenues sum to the total revenues" $
          Amount.sum (map revenueAmount taxesReportRevenues) == Just taxesReportTotalRevenues,
        declare "the third pillar contributions sum to the total third pillar contributions" $
          Amount.sum (map thirdPillarContributionCHFAmount taxesReportThirdPillarContributions) == Just taxesReportTotalThirdPillarContributions,
        validatePartitionedExpenses "insurance" insuranceExpenseCHFAmount taxesReportInsuranceExpenses,
        validatePartitionedExpenses "homeoffice" homeofficeExpenseCHFAmount taxesReportHomeofficeExpenses,
        validatePartitionedExpenses "electricity" electricityExpenseCHFAmount taxesReportElectricityExpenses,
        validatePartitionedExpenses "phone" phoneExpenseCHFAmount taxesReportPhoneExpenses,
        validatePartitionedExpenses "travel" travelExpenseCHFAmount taxesReportTravelExpenses,
        validatePartitionedExpenses "internet" internetExpenseCHFAmount taxesReportInternetExpenses,
        validatePartitionedExpenses "health insurance" healthExpenseCHFAmount taxesReportHealthExpenses
      ]

validatePartitionedExpenses ::
  String ->
  (a -> Money.Amount) ->
  PartitionedExpenses a ann ->
  Data.Validity.Validation
validatePartitionedExpenses expenseName businessAccessor PartitionedExpenses {..} =
  mconcat
    [ declare ("the " <> expenseName <> " costs sum to the total " <> expenseName <> " costs") $
        Amount.sum (map businessAccessor partitionedExpensesBusinessExpenses) == Just partitionedExpensesTotalBusinessExpenses,
      declare ("the private " <> expenseName <> " costs sum to the total private " <> expenseName <> " costs") $
        Amount.sum (map privateExpenseCHFAmount partitionedExpensesPrivateExpenses) == Just partitionedExpensesTotalPrivateExpenses
    ]

data PartitionedExpenses a ann = PartitionedExpenses
  { partitionedExpensesBusinessExpenses :: ![a],
    partitionedExpensesTotalBusinessExpenses :: !Money.Amount,
    partitionedExpensesPrivateExpenses :: ![PrivateExpense ann],
    partitionedExpensesTotalPrivateExpenses :: !Money.Amount
  }
  deriving (Show, Generic)

instance (Validity a, Validity ann, Show ann, Ord ann) => Validity (PartitionedExpenses a ann)

data PrivateExpense ann = PrivateExpense
  { privateExpenseTimestamp :: !Timestamp,
    privateExpenseDescription :: !Description,
    privateExpenseAmount :: !Money.Amount,
    privateExpenseCurrency :: !(Currency ann),
    privateExpenseCHFAmount :: !Money.Amount,
    privateExpenseEvidence :: ![Path Rel File]
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (PrivateExpense ann)

data AssetAccount ann = AssetAccount
  { assetAccountName :: !AccountName,
    assetAccountBalances :: !(Map (Currency ann) (Money.Amount, Money.Amount)),
    assetAccountConvertedBalance :: !Money.Amount,
    assetAccountAttachments :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (AssetAccount ann)

data Revenue ann = Revenue
  { revenueTimestamp :: !Timestamp,
    revenueDescription :: !Description,
    revenueAmount :: !Money.Amount,
    revenueCurrency :: !(Currency ann),
    revenueCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    revenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Revenue ann)

data ThirdPillarContribution ann = ThirdPillarContribution
  { thirdPillarContributionTimestamp :: !Timestamp,
    thirdPillarContributionDescription :: !Description,
    thirdPillarContributionCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    thirdPillarContributionEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (ThirdPillarContribution ann)

data InsuranceExpense ann = InsuranceExpense
  { insuranceExpenseTimestamp :: !Timestamp,
    insuranceExpenseDescription :: !Description,
    insuranceExpenseAmount :: !Money.Amount,
    insuranceExpenseCurrency :: !(Currency ann),
    insuranceExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    insuranceExpenseEvidence :: ![Path Rel File]
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (InsuranceExpense ann)

data HomeofficeExpense ann = HomeofficeExpense
  { homeofficeExpenseTimestamp :: !Timestamp,
    homeofficeExpenseDescription :: !Description,
    homeofficeExpenseAmount :: !Money.Amount,
    homeofficeExpenseCurrency :: !(Currency ann),
    homeofficeExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    homeofficeExpenseEvidence :: ![Path Rel File]
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (HomeofficeExpense ann)

data ElectricityExpense ann = ElectricityExpense
  { electricityExpenseTimestamp :: !Timestamp,
    electricityExpenseDescription :: !Description,
    electricityExpenseAmount :: !Money.Amount,
    electricityExpenseCurrency :: !(Currency ann),
    electricityExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    electricityExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (ElectricityExpense ann)

data PhoneExpense ann = PhoneExpense
  { phoneExpenseTimestamp :: !Timestamp,
    phoneExpenseDescription :: !Description,
    phoneExpenseAmount :: !Money.Amount,
    phoneExpenseCurrency :: !(Currency ann),
    phoneExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    phoneExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (PhoneExpense ann)

data TravelExpense ann = TravelExpense
  { travelExpenseTimestamp :: !Timestamp,
    travelExpenseDescription :: !Description,
    travelExpenseAmount :: !Money.Amount,
    travelExpenseCurrency :: !(Currency ann),
    travelExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    travelExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TravelExpense ann)

data InternetExpense ann = InternetExpense
  { internetExpenseTimestamp :: !Timestamp,
    internetExpenseDescription :: !Description,
    internetExpenseAmount :: !Money.Amount,
    internetExpenseCurrency :: !(Currency ann),
    internetExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    internetExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (InternetExpense ann)

data HealthExpense ann = HealthExpense
  { healthExpenseTimestamp :: !Timestamp,
    healthExpenseDescription :: !Description,
    healthExpenseAmount :: !Money.Amount,
    healthExpenseCurrency :: !(Currency ann),
    healthExpenseCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    healthExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (HealthExpense ann)

data DepreciationSchedule ann = DepreciationSchedule
  { depreciationScheduleDepreciationRate :: !(Ratio Natural),
    depreciationScheduleOpeningBalance :: !Money.Amount,
    depreciationScheduleOpeningBalanceEvidence :: !(NonEmpty (Path Rel File)),
    depreciationSchedulePurchases :: ![DepreciationPurchase ann],
    depreciationScheduleTotalPurchases :: !Money.Amount,
    depreciationScheduleDepreciation :: !Money.Amount,
    depreciationScheduleClosingBalance :: !Money.Amount
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (DepreciationSchedule ann)

data DepreciationPurchase ann = DepreciationPurchase
  { depreciationPurchaseTimestamp :: !Timestamp,
    depreciationPurchaseDescription :: !Description,
    depreciationPurchaseAmount :: !Money.Amount,
    depreciationPurchaseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (DepreciationPurchase ann)

data TaxesError ann
  = TaxesErrorNoCHF
  | TaxesErrorSum
  | TaxesErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  | TaxesErrorConvertError !(ConvertError ann)
  | TaxesErrorEvaluatedLedger !(EvaluatedLedgerError ann)
  | TaxesErrorBalanceError !(BalanceError ann)
  | TaxesErrorNoDescription
  | TaxesErrorNoEvidence !ann
  | TaxesErrorNegativeAsset !ann !Money.Account
  | TaxesErrorNegativeExpense !ann !Money.Account
  | TaxesErrorPositiveIncome !ann !ann !Money.Account
  | TaxesErrorCouldNotConvert !ann !(Currency ann) !(Currency ann) !Money.Amount
  | TaxesErrorAssetAccountWithoutEvidence !(GenLocated ann AccountName)
  | TaxesErrorRevenueWithoutEvidence !ann !ann !(GenLocated ann AccountName)
  | TaxesErrorDeductibleAndNotDeductible !ann !ann !ann
  | TaxesErrorRedundantlyDeclared !ann !ann !ann
  | TaxesErrorUntaggedExpenses !ann !(GenLocated ann (Ledger.Posting ann))
  | TaxesErrorDepreciationAdditionOverflow
  | TaxesErrorDepreciationFractionError
  | TaxesErrorDepreciationSubtractionOverflow
  | TaxesErrorDepreciationNegativeBalance !ann !AccountName
  | TaxesErrorDepreciationAccountNotDeclared !AccountName
  | TaxesErrorDepreciationNoOpeningBalance !ann !AccountName !Year
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TaxesError ann)

instance ToReport (TaxesError SourceSpan) where
  toReport = \case
    TaxesErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    TaxesErrorSum -> Err Nothing "amounts being summed became too big" [] []
    TaxesErrorWrongCHF (Located cdl _) ->
      Err
        Nothing
        "Incompatible CHF defined"
        [(toDiagnosePosition cdl, This "This currency declaration must use 0.01")]
        []
    TaxesErrorConvertError ce -> toReport ce
    TaxesErrorEvaluatedLedger ele -> toReport ele
    TaxesErrorBalanceError be -> toReport be
    TaxesErrorNoDescription -> Err Nothing "no description" [] []
    TaxesErrorNoEvidence tl ->
      Err
        Nothing
        "No evidence in transaction"
        [(toDiagnosePosition tl, This "This transaction is missing evidence")]
        []
    TaxesErrorNegativeAsset al _ ->
      Err
        Nothing
        "Negative asset amount"
        [ (toDiagnosePosition al, Where "in this account")
        ]
        []
    TaxesErrorNegativeExpense al _ ->
      Err
        Nothing
        "Negative expense amount"
        [ (toDiagnosePosition al, Where "in this account")
        ]
        []
    TaxesErrorPositiveIncome tl pl _ ->
      Err
        Nothing
        "Positive income amount"
        [ (toDiagnosePosition pl, Where "in this posting"),
          (toDiagnosePosition tl, Blank)
        ]
        []
    TaxesErrorCouldNotConvert al currencyFrom currencyTo _ ->
      let symbolFrom = currencySymbol currencyFrom
          symbolTo = currencySymbol currencyTo
       in Err
            Nothing
            ( unwords
                [ "could not convert from",
                  CurrencySymbol.toString symbolFrom,
                  "to",
                  CurrencySymbol.toString symbolTo
                ]
            )
            [(toDiagnosePosition al, This "this amount")]
            []
    TaxesErrorAssetAccountWithoutEvidence (Located anl an) ->
      Err
        Nothing
        "Asset account without evidence"
        [ (toDiagnosePosition anl, This "This account declaration has neither evidence nor is tagged as undeclared")
        ]
        [ Hint $
            unlines'
              [ "You can tag this account as undeclared:",
                T.unpack $
                  T.strip $
                    formatDeclaration $
                      DeclarationAccount $
                        noLoc $
                          AccountDeclaration
                            { accountDeclarationName = noLoc an,
                              accountDeclarationType = Nothing,
                              accountDeclarationExtras = [noLoc $ AccountExtraTag $ noLoc $ ExtraTag $ noLoc "undeclared"]
                            }
              ]
        ]
    TaxesErrorRevenueWithoutEvidence tl pl (Located anl an) ->
      Err
        Nothing
        "Revenue without evidence"
        [ (toDiagnosePosition pl, This "This revenue posting has no evidence eventhough its account is not tagged as undeclared"),
          (toDiagnosePosition tl, Where "in this transaction"),
          (toDiagnosePosition anl, Where "The account is defined here")
        ]
        [ Hint $
            unlines'
              [ "You can tag this account as undeclared:",
                T.unpack $
                  T.strip $
                    formatDeclaration $
                      DeclarationAccount $
                        noLoc $
                          AccountDeclaration
                            { accountDeclarationName = noLoc an,
                              accountDeclarationType = Nothing,
                              accountDeclarationExtras = [noLoc $ AccountExtraTag $ noLoc $ ExtraTag $ noLoc "undeclared"]
                            }
              ]
        ]
    TaxesErrorDeductibleAndNotDeductible tl tagl nottagl ->
      Err
        Nothing
        "Transaction marked as both deductible and not deductible"
        [ (toDiagnosePosition tagl, Where "Tagged as deductible"),
          (toDiagnosePosition nottagl, This "and as not deductible"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    TaxesErrorRedundantlyDeclared tl t1l t2l ->
      Err
        Nothing
        "Transaction marked as as either deductible or not deductible in multiple ways"
        [ (toDiagnosePosition t1l, Where "Tagged here"),
          (toDiagnosePosition t2l, This "and here"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    TaxesErrorUntaggedExpenses tl (Located pl _) ->
      Err
        Nothing
        "Expense not marked as either deductible or not-deductible"
        [ (toDiagnosePosition pl, This "This posting represents an expense that is neither tagged as deductible nor as not-deductible."),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        [Hint "tag as 'deductible' or 'not-deductible' to resolve this ambiguity"]
    TaxesErrorDepreciationAdditionOverflow -> Err Nothing "depreciation addition overflow: opening balance plus purchases became too big" [] []
    TaxesErrorDepreciationFractionError -> Err Nothing "depreciation fraction error: could not compute depreciation amount" [] []
    TaxesErrorDepreciationSubtractionOverflow -> Err Nothing "depreciation subtraction overflow: could not compute closing balance" [] []
    TaxesErrorDepreciationNegativeBalance al an ->
      Err
        Nothing
        ("Negative balance for depreciation account: " <> show an)
        [(toDiagnosePosition al, This "This account has a negative balance")]
        []
    TaxesErrorDepreciationAccountNotDeclared an ->
      Err
        Nothing
        ("Depreciation account not declared: " <> show an)
        []
        [Hint "Declare the account in the ledger"]
    TaxesErrorDepreciationNoOpeningBalance al an year ->
      Err
        Nothing
        ("No opening balance for depreciation account: " <> show an)
        [(toDiagnosePosition al, This "This account has no balance at the end of the previous year")]
        [ Hint $
            unlines'
              [ "Make sure the asset account has a balance at the end of the previous year.",
                "For example, add a transaction like this:",
                T.unpack $
                  T.strip $
                    formatDeclaration $
                      DeclarationTransaction $
                        noLoc $
                          Syntax.Transaction
                            { transactionTimestamp = noLoc $ TimestampDay $ fromGregorian (year - 1) 12 31,
                              transactionDescription = Just $ noLoc $ Description "Opening balance",
                              transactionPostings =
                                [ noLoc $
                                    Syntax.Posting
                                      { postingReal = True,
                                        postingAccountName = noLoc "equity:starting",
                                        postingAccount = noLoc $ DecimalLiteral Nothing 0 0,
                                        postingCurrencySymbol = noLoc $ CurrencySymbol "CHF",
                                        postingCost = Nothing,
                                        postingRatio = Nothing
                                      },
                                  noLoc $
                                    Syntax.Posting
                                      { postingReal = True,
                                        postingAccountName = noLoc an,
                                        postingAccount = noLoc $ DecimalLiteral Nothing 0 0,
                                        postingCurrencySymbol = noLoc $ CurrencySymbol "CHF",
                                        postingCost = Nothing,
                                        postingRatio = Nothing
                                      }
                                ],
                              transactionExtras = []
                            }
              ]
        ]

unlines' :: [String] -> String
unlines' = intercalate "\n"
