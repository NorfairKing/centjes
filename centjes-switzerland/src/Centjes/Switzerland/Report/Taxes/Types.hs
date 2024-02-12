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
    Revenue (..),
    TaxesError (..),
  )
where

import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.Amount as Amount
import qualified Money.Amount as Money (Amount)
import Money.QuantisationFactor as Money (QuantisationFactor (..))
import Numeric.Natural
import Path

-- | The settings we need to produce a 'TaxesReport'
data TaxesInput = TaxesInput
  { taxesInputLastName :: !Text,
    taxesInputFirstName :: !Text,
    taxesInputYear :: !Year,
    taxesInputInsuredPersonNumber :: !Text,
    taxesInputIncomeAccounts :: ![AccountName],
    taxesInputVATIncomeAccountName :: !AccountName
  }
  deriving (Show, Eq, Generic)

instance Validity TaxesInput

-- | The information we need to produce Taxes reports like the pdfs, zip files,
-- or xml files.
data TaxesReport ann = TaxesReport
  { taxesReportLastName :: !Text,
    taxesReportFirstName :: !Text,
    taxesReportYear :: !Year,
    taxesReportInsuredPersonNumber :: !Text,
    taxesReportCHF :: !(Currency ann),
    taxesReportRevenues :: ![Revenue ann],
    -- TODO allow the user to distinguish between self-employment and other revenue somehow.
    taxesReportSelfEmploymentRevenue :: !Money.Amount
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TaxesReport ann) where
  validate vr@TaxesReport {..} =
    mconcat
      [ genericValidate vr,
        declare "The self-employement revenue is the sum of the netto revenues." $
          Amount.sum (map revenueNettoCHFAmount taxesReportRevenues)
            == Just taxesReportSelfEmploymentRevenue
      ]

data TaxesError ann
  = TaxesErrorNoCHF
  | TaxesErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  | TaxesErrorNoDescription
  | TaxesErrorNoEvidence !ann
  | TaxesErrorCouldNotConvert !ann !(Currency ann) !(Currency ann) !Money.Amount
  | TaxesErrorPositiveIncome !ann !ann !Money.Account
  | TaxesErrorNoVATPosting
  | TaxesErrorVATPostingNotVATAccount
  | TaxesErrorNoVATPercentage !ann
  | TaxesErrorUnknownVATRate !ann !ann !(Ratio Natural)
  | TaxesErrorSum ![Money.Amount]
  | TaxesErrorAdd !Money.Amount !Money.Amount
  | TaxesErrorSubtract !Money.Amount !Money.Amount
  | TaxesErrorReportInvalid !(TaxesReport ann) !String
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TaxesError ann)

instance ToReport (TaxesError SourceSpan) where
  toReport = \case
    TaxesErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    TaxesErrorWrongCHF (Located cdl _) ->
      Err
        Nothing
        "Incompatible CHF defined"
        [(toDiagnosePosition cdl, This "This currency declaration must use 0.01")]
        []

-- Note that this is a separate type from the DomesticRevenue and
-- ForeignRevenue types because in the VATReport we have to make sure that the
-- foreign revenues never contain VAT.
data Revenue ann = Revenue
  { revenueTimestamp :: !Timestamp,
    revenueDescription :: !Description,
    revenueGrossAmount :: !Money.Amount,
    revenueCurrency :: !(Currency ann),
    revenueGrossCHFAmount :: !Money.Amount,
    -- | Just, if VAT was charged and how much
    revenueVAT :: !(Maybe (VATRevenue ann)),
    -- | Same as gross if no VAT was charged.
    -- Gross - VAT if there was.
    revenueNettoCHFAmount :: !Money.Amount,
    -- | Evidence in tarball
    revenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Revenue ann) where
  validate r@Revenue {..} =
    mconcat
      [ genericValidate r,
        declare "The netto CHF amount is the gross minus VAT" $
          case revenueVAT of
            Nothing -> revenueGrossCHFAmount == revenueNettoCHFAmount
            Just VATRevenue {..} ->
              Amount.subtract revenueGrossCHFAmount vatRevenueCHFAmount
                == Just revenueNettoCHFAmount
      ]

-- Note that VAT must be charged in the same currency as the revenue.
data VATRevenue ann = VATRevenue
  { vatRevenueAmount :: !Money.Amount,
    vatRevenueCHFAmount :: !Money.Amount
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (VATRevenue ann)
