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
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time

taxesReportInput :: TaxesReport ann -> Input
taxesReportInput TaxesReport {..} =
  let inputLastName = taxesReportLastName
      inputFirstName = taxesReportFirstName
      inputYear = taxesReportYear
   in Input {..}

-- Note that this is a separate type from the ETax 'XMLReport' because there
-- is more information in the README than there is in the ETax
data Input = Input
  { inputLastName :: Text,
    inputFirstName :: Text,
    inputYear :: !Year
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
