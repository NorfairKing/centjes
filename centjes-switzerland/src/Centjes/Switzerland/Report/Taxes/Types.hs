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
    TaxesReport (..),
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
    TaxesError (..),
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Convert
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format
import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Module
import Centjes.Report.Balance
import qualified Centjes.Tag as Tag
import Centjes.Validation
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
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
    taxesInputHomeofficeExpensesAccount :: !AccountName,
    taxesInputAccidentInsuranceExpenseAccount :: !AccountName,
    taxesInputDailyAllowanceInsuranceExpenseAccount :: !AccountName,
    taxesInputElectricityExpensesAccount :: !AccountName,
    taxesInputPhoneExpensesAccount :: !AccountName,
    taxesInputTravelExpensesAccount :: !AccountName,
    taxesInputInternetExpensesAccount :: !AccountName,
    taxesInputHealthExpensesAccount :: !AccountName
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
  taxesInputHomeofficeExpensesAccount <-
    setting
      [ help "the account to use for homeoffice expenses",
        reader $ maybeReader AccountName.fromString,
        conf "homeoffice-expenses-account",
        value "expenses:rent:homeoffice"
      ]
  taxesInputElectricityExpensesAccount <-
    setting
      [ help "the account to use for electricity expenses",
        reader $ maybeReader AccountName.fromString,
        conf "electricity-expenses-account",
        value "expenses:electricity:homeoffice"
      ]
  taxesInputPhoneExpensesAccount <-
    setting
      [ help "the account to use for phone expenses",
        reader $ maybeReader AccountName.fromString,
        conf "phone-expenses-account",
        value "expenses:phone:professional"
      ]
  taxesInputTravelExpensesAccount <-
    setting
      [ help "the account to use for travel expenses",
        reader $ maybeReader AccountName.fromString,
        conf "travel-expenses-account",
        value "expenses:travel:professional"
      ]
  taxesInputInternetExpensesAccount <-
    setting
      [ help "the account to use for internet expenses",
        reader $ maybeReader AccountName.fromString,
        conf "internet-expenses-account",
        value "expenses:internet:homeoffice"
      ]
  taxesInputHealthExpensesAccount <-
    setting
      [ help "the account to use for health insurance expenses",
        reader $ maybeReader AccountName.fromString,
        conf "health-insurance-expenses-account",
        value "expenses:health"
      ]
  pure TaxesInput {..}

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
    taxesReportInsuranceExpenses :: ![InsuranceExpense ann],
    taxesReportTotalInsuranceExpenses :: !Money.Amount,
    taxesReportHomeofficeExpenses :: ![HomeofficeExpense ann],
    taxesReportTotalHomeofficeExpenses :: !Money.Amount,
    taxesReportElectricityExpenses :: ![ElectricityExpense ann],
    taxesReportTotalElectricityExpenses :: !Money.Amount,
    taxesReportPhoneExpenses :: ![PhoneExpense ann],
    taxesReportTotalPhoneExpenses :: !Money.Amount,
    taxesReportTravelExpenses :: ![TravelExpense ann],
    taxesReportTotalTravelExpenses :: !Money.Amount,
    taxesReportInternetExpenses :: ![InternetExpense ann],
    taxesReportTotalInternetExpenses :: !Money.Amount,
    taxesReportHealthExpenses :: ![HealthExpense ann],
    taxesReportTotalHealthExpenses :: !Money.Amount
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
        declare "the insurance costs sum to the total insurance costs" $
          Amount.sum (map insuranceExpenseAmount taxesReportInsuranceExpenses) == Just taxesReportTotalInsuranceExpenses,
        declare "the homeoffice costs sum to the total homeoffice costs" $
          Amount.sum (map homeofficeExpenseAmount taxesReportHomeofficeExpenses) == Just taxesReportTotalHomeofficeExpenses,
        declare "the electricity costs sum to the total electricity costs" $
          Amount.sum (map electricityExpenseAmount taxesReportElectricityExpenses) == Just taxesReportTotalElectricityExpenses,
        declare "the phone costs sum to the total phone costs" $
          Amount.sum (map phoneExpenseAmount taxesReportPhoneExpenses) == Just taxesReportTotalPhoneExpenses,
        declare "the travel costs sum to the total travel costs" $
          Amount.sum (map travelExpenseAmount taxesReportTravelExpenses) == Just taxesReportTotalTravelExpenses,
        declare "the internet costs sum to the total internet costs" $
          Amount.sum (map internetExpenseAmount taxesReportInternetExpenses) == Just taxesReportTotalInternetExpenses,
        declare "the health insurance costs sum to the total health insurance costs" $
          Amount.sum (map healthExpenseAmount taxesReportHealthExpenses) == Just taxesReportTotalHealthExpenses
      ]

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

data TaxesError ann
  = TaxesErrorNoCHF
  | TaxesErrorSum
  | TaxesErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  | TaxesErrorConvertError !(ConvertError ann)
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

unlines' :: [String] -> String
unlines' = intercalate "\n"
