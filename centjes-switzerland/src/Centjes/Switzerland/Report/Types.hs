{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

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
import qualified Money.Amount as Amount
import qualified Money.Amount as Money (Amount)
import Money.QuantisationFactor as Money (QuantisationFactor (..))
import Path

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
