{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Types
  ( TaxesInput (..),
    TaxesReport (..),
    TaxesError (..),
  )
where

import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Data.Text (Text)
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import Money.QuantisationFactor as Money (QuantisationFactor (..))

-- | The settings we need to produce a 'TaxesReport'
data TaxesInput = TaxesInput
  { taxesInputPersonName :: !Text,
    taxesInputYear :: !Int
  }
  deriving (Show, Eq, Generic)

instance Validity TaxesInput

-- | The information we need to produce Taxes reports like the pdfs, zip files,
-- or xml files.
data TaxesReport ann = TaxesReport
  { taxesReportPersonName :: !Text,
    taxesReportYear :: !Int,
    taxesReportCHF :: !(Currency ann)
  }
  deriving (Show, Eq, Generic)

instance (Validity ann) => Validity (TaxesReport ann) where
  validate vr@TaxesReport {} =
    mconcat
      [ genericValidate vr
      ]

data TaxesError ann
  = TaxesErrorNoCHF
  | TaxesErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  deriving (Show, Eq, Generic)

instance (Validity ann) => Validity (TaxesError ann)

instance ToReport (TaxesError SourceSpan) where
  toReport = \case
    TaxesErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    TaxesErrorWrongCHF (Located cdl _) ->
      Err
        Nothing
        "Incompatible CHF defined"
        [(toDiagnosePosition cdl, This "This currency declaration must use 0.01")]
        []
