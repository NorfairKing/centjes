{-# LANGUAGE DeriveGeneric #-}

module Centjes.Switzerland.Report.Types where

import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import qualified Money.Amount as Money (Amount)
import Money.QuantisationFactor as Money (QuantisationFactor (..))
import Path

data Revenue ann = Revenue
  { revenueTimestamp :: !Timestamp,
    revenueDescription :: !Description,
    revenueGrossAmount :: !Money.Amount,
    revenueCurrency :: !(Currency ann),
    revenueCHFAmount :: !Money.Amount,
    -- | Just, if VAT was charged and how much
    revenueVAT :: !(Maybe (VATRevenue ann)),
    -- | Same as gross if no VAT was charged.
    -- Gross - VAT if there was.
    revenueNettoAmount :: !Money.Amount,
    -- | Evidence in tarball
    revenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Revenue ann)

data VATRevenue ann = VATRevenue
  { vatRevenueCurrency :: !(Currency ann),
    vatRevenueAmount :: !Money.Amount,
    vatRevenueCHFAmount :: !Money.Amount
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (VATRevenue ann)
