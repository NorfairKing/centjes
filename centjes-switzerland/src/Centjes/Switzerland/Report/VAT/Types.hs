{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VAT.Types
  ( VATReport (..),
    DomesticRevenue (..),
    ForeignRevenue (..),
    DeductibleExpense (..),
    VATRate (..),
    VATError (..),
  )
where

import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Text (Text)
import Data.Time.Calendar.Quarter
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import Money.Account as Money (Account (..))
import qualified Money.Account as Account
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import Numeric.Natural
import Path

-- TODO upstream this to validity-time
deriving instance Generic Quarter

instance Validity Quarter

data VATReport ann = VATReport
  { vatReportName :: !Text,
    vatReportQuarter :: !Quarter,
    vatReportCHF :: !(Currency ann),
    vatReportDomesticRevenues :: ![DomesticRevenue ann],
    vatReportForeignRevenues :: ![ForeignRevenue ann],
    vatReportDeductibleExpenses :: ![DeductibleExpense ann],
    -- | 200
    --
    -- Total der vereinbarten bzw. vereinnahmten Entgelte, inkl.
    -- optierte Leistungen, Entgelte aus Übertragungen im
    -- Meldeverfahren sowie aus Leistungen im Ausland
    -- (weltweiter Umsatz)
    vatReportTotalRevenue :: !Money.Amount,
    -- | 221
    --
    -- Leistungen im Ausland (Ort der Leistung im Ausland)
    vatReportTotalForeignRevenue :: !Money.Amount,
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
    -- | 500
    --
    -- Zu bezahlender Betrag
    vatReportPayable :: !Money.Account
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (VATReport ann) where
  validate vr@VATReport {..} =
    mconcat
      [ genericValidate vr,
        declare "The total domestic revenue is the total of the domestic revenues" $
          Amount.sum (map domesticRevenueCHFAmount vatReportDomesticRevenues)
            == Just vatReportTotalDomesticRevenue,
        declare "The total fereign revenue is the total of the foreign revenues" $
          Amount.sum (map foreignRevenueCHFAmount vatReportForeignRevenues)
            == Just vatReportTotalForeignRevenue,
        declare
          "The total revenue is the sum of domestic and foreign"
          $ Amount.add
            vatReportTotalDomesticRevenue
            vatReportTotalForeignRevenue
            == Just vatReportTotalRevenue,
        declare "The total 2023 standard rate VAT revenue is the total of VAT amounts of the revenues" $
          Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2023Standard) . domesticRevenueVATRate) vatReportDomesticRevenues))
            == Just vatReport2023StandardRateVATRevenue,
        declare "The total 2024 standard rate VAT revenue is the total of VAT amounts of the revenues" $
          Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2024Standard) . domesticRevenueVATRate) vatReportDomesticRevenues))
            == Just vatReport2024StandardRateVATRevenue,
        declare "The total vat is the sum of all the vat fields" $
          Amount.sum [vatReport2023StandardRateVATRevenue, vatReport2024StandardRateVATRevenue] == Just vatReportTotalVATRevenue,
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
  deriving (Show, Eq, Generic)

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
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (ForeignRevenue ann)

data DeductibleExpense ann = DeductibleExpense
  { deductibleExpenseTimestamp :: !Timestamp,
    deductibleExpenseDescription :: !Description,
    deductibleExpenseAmount :: !Money.Amount,
    deductibleExpenseCurrency :: !(Currency ann),
    deductibleExpenseCHFAmount :: !Money.Amount,
    deductibleExpenseVATAmount :: !Money.Amount,
    deductibleExpenseVATCurrency :: !(Currency ann),
    deductibleExpenseVATCHFAmount :: !Money.Amount,
    deductibleExpenseVATRate :: !VATRate,
    -- | Evidence in tarball
    deductibleExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (DeductibleExpense ann)

data VATError ann
  = VATErrorNoCHF
  | VATErrorNoDescription
  | VATErrorNoEvidence !ann
  | VATErrorCouldNotConvert !ann !(Currency ann) !(Currency ann) !Money.Amount
  | VATErrorPositiveIncome !ann !ann !Money.Account
  | VATErrorNegativeExpense !ann !ann !Money.Account
  | VATErrorNoVATPosting
  | VATErrorVATPostingNotVATAccount
  | VATErrorNoVATPercentage
  | VATErrorUnknownVATRate !ann !ann !(Ratio Natural)
  | VATErrorSum ![Money.Amount]
  | VATErrorAdd !Money.Amount !Money.Amount
  | VATErrorSubtract !Money.Amount !Money.Amount
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (VATError ann)

instance ToReport (VATError SourceSpan) where
  toReport = \case
    VATErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
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
    VATErrorNoVATPosting -> Err Nothing "No VAT posting for domestic income" [] []
    VATErrorVATPostingNotVATAccount -> Err Nothing "VAT posting for domestic income had unknown account name" [] []
    VATErrorNoVATPercentage -> Err Nothing "VAT posting for domestic income did not have a percentage" [] []
    VATErrorUnknownVATRate tl pl _ ->
      Err
        Nothing
        "Unknown VAT rate"
        [ (toDiagnosePosition pl, This "in this percentage"),
          (toDiagnosePosition tl, Where "in this transaction")
        ]
        []
    VATErrorSum _ -> Err Nothing "could not sum amounts because the result would get too big" [] []
    VATErrorAdd _ _ -> Err Nothing "could not add amounts because the result wolud get too big" [] []
    VATErrorSubtract _ _ -> Err Nothing "Could not subtract amounts because the result wolud get too big or too small" [] []