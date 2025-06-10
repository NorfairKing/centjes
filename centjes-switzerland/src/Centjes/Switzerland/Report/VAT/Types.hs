{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VAT.Types
  ( VATInput (..),
    VATReport (..),
    DomesticRevenue (..),
    ForeignRevenue (..),
    DeductibleExpense (..),
    VATRate (..),
    VATError (..),
  )
where

import Autodocodec
import qualified Centjes.AccountName as AccountName
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Tag as Tag
import Centjes.Validation
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.Quarter
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import Money.Account as Money (Account (..))
import qualified Money.Account as Account
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import Money.QuantisationFactor as Money (QuantisationFactor (..))
import OptEnvConf
import Path
import Text.Read (readMaybe)
import Text.Show.Pretty

-- TODO upstream this to validity-time
deriving instance Generic Quarter

instance Validity Quarter

-- | The settings we need to produce a 'VATReport'
data VATInput = VATInput
  { vatInputPersonName :: !Text,
    vatInputOrganisationName :: !Text,
    vatInputVATId :: !Text,
    vatInputQuarter :: !Quarter,
    vatInputTagDeductible :: !Tag,
    vatInputTagNotDeductible :: !Tag,
    vatInputTagVATDeductible :: !Tag,
    vatInputTagNotVATDeductible :: !Tag,
    vatInputDomesticIncomeAccountName :: !AccountName,
    vatInputExportsIncomeAccountName :: !AccountName,
    vatInputForeignIncomeAccountName :: !AccountName,
    vatInputVATIncomeAccountName :: !AccountName,
    vatInputVATExpensesAccountName :: !AccountName
  }
  deriving (Show, Generic)

instance Validity VATInput

instance HasParser VATInput where
  settingsParser = parseVATInput

{-# ANN parseVATInput ("NOCOVER" :: String) #-}
parseVATInput :: Parser VATInput
parseVATInput = do
  vatInputPersonName <-
    choice
      [ setting
          [ help "Your legal name",
            conf "person-name"
          ],
        (\f l -> T.pack (unwords [f, l]))
          <$> setting
            [ help "your first name",
              conf "first-name"
            ]
          <*> setting
            [ help "your last name",
              conf "last-name"
            ]
      ]
  vatInputOrganisationName <-
    setting
      [ help "The organisation's legal name",
        conf "organisation-name"
      ]
  vatInputVATId <-
    setting
      [ help "the VAT identifier",
        conf "vat-id",
        example "111.222.333"
      ]
  vatInputQuarter <-
    choice
      [ setting
          [ help "the quarter to produce the report for",
            option,
            reader $ maybeReader readMaybe,
            long "quarter",
            confWith "quarter" $ codecViaAeson "Quarter",
            metavar "YYYY-QN"
          ],
        runIO $ dayQuarter . utctDay <$> getCurrentTime
      ]
  vatInputTagDeductible <-
    setting
      [ help "tag to use for deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-deductible",
        value "deductible"
      ]
  vatInputTagNotDeductible <-
    setting
      [ help "tag to use for non-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-deductible",
        value "not-deductible"
      ]
  vatInputTagVATDeductible <-
    setting
      [ help "tag to use for VAT-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-vat-deductible",
        value "vat-deductible"
      ]
  vatInputTagNotVATDeductible <-
    setting
      [ help "tag to use for non-VAT-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-vat-deductible",
        value "not-vat-deductible"
      ]
  vatInputDomesticIncomeAccountName <-
    setting
      [ help "Account name of your domestic income",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "domestic-income-account",
        value "income:domestic"
      ]
  vatInputExportsIncomeAccountName <-
    setting
      [ help "Account name of your exports' income",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "exports-income-account",
        value "income:exports"
      ]
  vatInputForeignIncomeAccountName <-
    setting
      [ help "Account name of your foreign income",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "foreign-income-account",
        value "income:foreign"
      ]
  vatInputVATIncomeAccountName <-
    setting
      [ help "Account name of your the VAT you've charged",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "vat-income-account",
        value "income:vat"
      ]
  vatInputVATExpensesAccountName <-
    setting
      [ help "Account name of your the VAT you've paid",
        reader $ eitherReader AccountName.fromStringOrError,
        conf "vat-expenses-account",
        value "expenses:vat"
      ]

  pure VATInput {..}

-- | The information we need to produce VAT reports like the pdfs, zip files,
-- or xml files.
data VATReport ann = VATReport
  { vatReportPersonName :: !Text,
    vatReportOrganisationName :: !Text,
    vatReportVATId :: !Text,
    vatReportQuarter :: !Quarter,
    vatReportCHF :: !(Currency ann),
    -- | Domestic revenues
    --
    -- These are the numbers that go into
    -- vatReportTotalDomesticRevenue
    -- ziffer 299
    vatReportDomesticRevenues :: ![DomesticRevenue ann],
    -- | Exports to foreign countries
    --
    -- This is work FOR foreign countries but not IN foreign countries
    --
    -- These are the numbers that go into
    -- vatReportTotalForeignRevenue
    -- ziffer 220
    vatReportExportsRevenues :: ![ForeignRevenue ann],
    -- | Foreign revenues
    --
    -- This is for work IN foreign countries
    --
    -- These are the numbers that go into
    -- vatReportTotalForeignRevenue
    -- ziffer 221
    vatReportForeignRevenues :: ![ForeignRevenue ann],
    vatReportDeductibleExpenses :: ![DeductibleExpense ann],
    -- | 200
    --
    -- Total der vereinbarten bzw. vereinnahmten Entgelte, inkl.
    -- optierte Leistungen, Entgelte aus Übertragungen im
    -- Meldeverfahren sowie aus Leistungen im Ausland
    -- (weltweiter Umsatz)
    vatReportTotalRevenue :: !Money.Amount,
    -- | 220
    --
    -- Leistungen ins Ausland
    vatReportTotalExportsRevenue :: !Money.Amount,
    -- | 221
    --
    -- Leistungen im Ausland (Ort der Leistung im Ausland)
    vatReportTotalForeignRevenue :: !Money.Amount,
    -- | 289
    --
    -- Total Abzüge: Ziﬀer 220 bis 280
    vatReportTotalForeignDeductions :: !Money.Amount,
    -- | 299
    --
    -- Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)
    vatReportTotalDomesticRevenue :: !Money.Amount,
    -- | 302
    --
    -- Leistungen zum Normalsatz 7.7%
    vatReportDomesticRevenue2023 :: !Money.Amount,
    vatReport2023StandardRateVATRevenue :: !Money.Amount,
    -- | 303
    --
    -- Leistungen zum Normalsatz 8.1%
    vatReportDomesticRevenue2024 :: !Money.Amount,
    vatReport2024StandardRateVATRevenue :: !Money.Amount,
    -- | 399
    --
    -- Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)
    vatReportTotalVATRevenue :: !Money.Amount,
    -- | 405
    --
    -- Vorsteuer auf Investitionen und übrigem Betriebsaufwand
    vatReportPaidVAT :: !Money.Amount,
    -- | 479
    --
    -- Totale Abzuge
    vatReportTotalVATDeductions :: !Money.Amount,
    -- | 500
    --
    -- Zu bezahlender Betrag
    vatReportPayable :: !Money.Account
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (VATReport ann) where
  validate vr@VATReport {..} =
    mconcat
      [ genericValidate vr,
        declare "The total domestic revenue is the total of the domestic revenues" $
          Amount.sum (map domesticRevenueCHFAmount vatReportDomesticRevenues)
            == Just vatReportTotalDomesticRevenue,
        declare "The total export revenue is the total of the exports " $
          Amount.sum (map foreignRevenueCHFAmount vatReportExportsRevenues)
            == Just vatReportTotalExportsRevenue,
        declare "The total foreign revenue is the total of the foreign revenues" $
          Amount.sum (map foreignRevenueCHFAmount vatReportForeignRevenues)
            == Just vatReportTotalForeignRevenue,
        declare "The total foreign deductions is the sum of foreign revenues and exports" $
          Amount.add vatReportTotalExportsRevenue vatReportTotalForeignRevenue == Just vatReportTotalForeignDeductions,
        declare
          "The total revenue is the sum of domestic, exports, and foreign"
          $ Amount.add
            vatReportTotalDomesticRevenue
            vatReportTotalForeignDeductions
            == Just vatReportTotalRevenue,
        declare "The total 2023 standard rate VAT revenue is the total of VAT amounts of the revenues" $
          Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2023Standard) . domesticRevenueVATRate) vatReportDomesticRevenues))
            == Just vatReport2023StandardRateVATRevenue,
        declare "The total 2024 standard rate VAT revenue is the total of VAT amounts of the revenues" $
          Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2024Standard) . domesticRevenueVATRate) vatReportDomesticRevenues))
            == Just vatReport2024StandardRateVATRevenue,
        declare "The total vat is the sum of all the vat fields" $
          Amount.sum [vatReport2023StandardRateVATRevenue, vatReport2024StandardRateVATRevenue] == Just vatReportTotalVATRevenue,
        declare "The total vat deductions is the sum of all vat deductions" $
          Amount.sum [vatReportPaidVAT] == Just vatReportTotalVATDeductions,
        declare "The payable amount is the VAT revenue minus the paid VAT" $
          Account.subtract (Account.fromAmount vatReportTotalVATRevenue) (Account.fromAmount vatReportPaidVAT) == Just vatReportPayable
      ]

data DomesticRevenue ann = DomesticRevenue
  { domesticRevenueTimestamp :: !Timestamp,
    domesticRevenueDescription :: !Description,
    domesticRevenueAmount :: !Money.Amount,
    domesticRevenueCurrency :: !(Currency ann),
    domesticRevenueCHFAmount :: !Money.Amount,
    domesticRevenueVATAmount :: !Money.Amount,
    domesticRevenueVATCurrency :: !(Currency ann),
    domesticRevenueVATCHFAmount :: !Money.Amount,
    domesticRevenueVATRate :: !VATRate,
    -- | Evidence in tarball
    domesticRevenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (DomesticRevenue ann)

-- TODO check that the rate is valid for the day?
data VATRate
  = -- | 7.7%
    VATRate2023Standard
  | -- | 8.1%
    VATRate2024Standard
  | -- | 2.5%
    VATRate2023Reduced
  | -- | 2.6%
    VATRate2024Reduced
  | -- | 3.7%
    VATRate2023Hotel
  | -- | 3.8%
    VATRate2024Hotel
  deriving (Show, Eq, Generic)

instance Validity VATRate

data ForeignRevenue ann = ForeignRevenue
  { foreignRevenueTimestamp :: !Timestamp,
    foreignRevenueDescription :: !Description,
    foreignRevenueAmount :: !Money.Amount,
    foreignRevenueCurrency :: !(Currency ann),
    foreignRevenueCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    foreignRevenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (ForeignRevenue ann)

data DeductibleExpense ann = DeductibleExpense
  { deductibleExpensePosting :: !ann,
    deductibleExpenseTimestamp :: !Timestamp,
    deductibleExpenseDescription :: !Description,
    deductibleExpenseAmount :: !Money.Amount,
    deductibleExpenseCurrency :: !(Currency ann),
    deductibleExpenseCHFAmount :: !Money.Amount,
    deductibleExpenseVATAmount :: !Money.Amount,
    deductibleExpenseVATCurrency :: !(Currency ann),
    deductibleExpenseVATCHFAmount :: !Money.Amount,
    deductibleExpenseVATRate :: !Rational,
    -- | Evidence in tarball
    deductibleExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (DeductibleExpense ann)

data VATError ann
  = VATErrorNoCHF
  | VATErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  | VATErrorNoTagDeductible
  | VATErrorNoTagNotDeductible
  | VATErrorNoDescription
  | VATErrorNoEvidence !ann
  | VATErrorCouldNotConvert !ann !(Currency ann) !(Currency ann) !Money.Amount
  | VATErrorPositiveIncome !ann !ann !Money.Account
  | VATErrorNegativeExpense !ann !ann !Money.Account
  | VATErrorNoVATPosting !ann !ann
  | VATErrorVATPostingNotVATAccount !ann !ann
  | VATErrorNoVATPercentage !ann
  | VATErrorUnknownVATRate !ann !ann !Rational
  | VATErrorDeductibleAndNotDeductible !ann !ann !ann
  | VATErrorRedundantlyDeclared !ann !ann !ann
  | VATErrorDeductibleNoExpenses !ann !ann
  | VATErrorUntaggedExpenses !ann !(GenLocated ann (Posting ann))
  | VATErrorSum ![Money.Amount]
  | VATErrorAdd !Money.Amount !Money.Amount
  | VATErrorSubtract !Money.Amount !Money.Amount
  | VATErrorReportInvalid !(VATReport ann) !String
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (VATError ann)

instance ToReport (VATError SourceSpan) where
  toReport = \case
    VATErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    VATErrorWrongCHF (Located cdl _) ->
      Err
        Nothing
        "Incompatible CHF defined"
        [(toDiagnosePosition cdl, This "This currency declaration must use 0.01")]
        []
    VATErrorNoTagDeductible -> Err Nothing "no tag 'deductible' declared" [] []
    VATErrorNoTagNotDeductible -> Err Nothing "no tag 'not-deductible' declared" [] []
    VATErrorNoDescription -> Err Nothing "no description" [] []
    VATErrorNoEvidence tl ->
      Err
        Nothing
        "No evidence in transaction"
        [(toDiagnosePosition tl, This "This transaction is missing evidence")]
        []
    VATErrorCouldNotConvert al currencyFrom currencyTo _ ->
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
    VATErrorPositiveIncome tl pl _ ->
      Err
        Nothing
        "Positive income amount"
        [ (toDiagnosePosition pl, Where "in this posting"),
          (toDiagnosePosition tl, Blank)
        ]
        []
    VATErrorNegativeExpense tl pl _ ->
      Err
        Nothing
        "Negative expense amount"
        [ (toDiagnosePosition pl, Where "in this posting"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorNoVATPosting tl pl ->
      Err
        Nothing
        "Missing VAT posting"
        [ (toDiagnosePosition pl, This "after this posting"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorVATPostingNotVATAccount tl pl ->
      Err
        Nothing
        "What should have been a VAT posting for had unrecognised account name"
        [ (toDiagnosePosition pl, This "this posting"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorNoVATPercentage tl ->
      Err
        Nothing
        "VAT posting for domestic income did not have a percentage"
        [ (toDiagnosePosition tl, This "in this transaction")
        ]
        []
    VATErrorUnknownVATRate tl pl _ ->
      Err
        Nothing
        "Unknown VAT rate"
        [ (toDiagnosePosition pl, This "in this percentage"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorDeductibleAndNotDeductible tl tagl nottagl ->
      Err
        Nothing
        "Transaction marked as both deductible and not deductible"
        [ (toDiagnosePosition tagl, Where "Tagged as deductible"),
          (toDiagnosePosition nottagl, This "and as not deductible"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorRedundantlyDeclared tl t1l t2l ->
      Err
        Nothing
        "Transaction marked as as either deductible or not deductible in multiple ways"
        [ (toDiagnosePosition t1l, Where "Tagged here"),
          (toDiagnosePosition t2l, This "and here"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorDeductibleNoExpenses tl tagl ->
      Err
        Nothing
        "Transaction marked as deductible but contained no expenses"
        [ (toDiagnosePosition tagl, This "Tagged as deductible"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorUntaggedExpenses tl (Located pl _) ->
      Err
        Nothing
        "Expense not marked as either deductible or not-deductible"
        [ (toDiagnosePosition pl, This "This posting represents an expense that is neither tagged as deductible nor as not-deductible."),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorSum _ -> Err Nothing "could not sum amounts because the result would get too big" [] []
    VATErrorAdd _ _ -> Err Nothing "could not add amounts because the result wolud get too big" [] []
    VATErrorSubtract _ _ -> Err Nothing "Could not subtract amounts because the result wolud get too big or too small" [] []
    VATErrorReportInvalid report e ->
      Err
        Nothing
        ( unlines
            [ "Produced VATReport is considered invalid.",
              "This indicates a bug in this program.",
              ppShow report,
              e
            ]
        )
        []
        []
