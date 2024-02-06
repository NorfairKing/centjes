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
import Centjes.Switzerland.Report.Taxes.Types
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time

taxesReportInput :: TaxesReport ann -> Input
taxesReportInput TaxesReport {..} =
  let inputPersonName = taxesReportPersonName
      inputYear = taxesReportYear
   in Input {..}

-- Note that this is a separate type from the ETax 'XMLReport' because there
-- is more information in the README than there is in the ETax
data Input = Input
  { inputPersonName :: Text,
    inputYear :: !Year
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "person_name" "person name"
          .= inputPersonName
        <*> requiredField "year" "year"
          .= inputYear
