{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes.ETax
  ( -- * XML Report
    produceXMLReport,
    xmlReportDocument,
    xmlRenderSettings,

    -- ** Types
    XMLReport (..),
  )
where

import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.XML
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Text.XML as XML

-- | `taxDeclaration`
data XMLReport = XMLReport
  deriving (Show)

instance ToElement XMLReport where
  toElement XMLReport {} =
    XML.Element
      { elementName = ech0119Name "taxDeclaration",
        elementAttributes =
          M.fromList
            [ (xsiName "schemaLocation", "http://www.ech.ch/xmlns/eCH-0119/4 eCH-0119-4-0_draft.xsd")
            ],
        elementNodes = []
      }

-- | Produce an 'XMLReport' from a 'TaxesReport' at the given time.
--
-- TODO Put this in a validation instead of a Maybe?
produceXMLReport :: UTCTime -> TaxesReport ann -> Maybe XMLReport
produceXMLReport _ TaxesReport {} = do
  pure XMLReport {}

-- | Produce an XML Document from the XMLReport.
xmlReportDocument :: XMLReport -> XML.Document
xmlReportDocument xmlReport =
  XML.Document
    { documentPrologue =
        XML.Prologue
          { prologueBefore = [],
            prologueDoctype = Nothing,
            prologueAfter = []
          },
      documentRoot = toElement xmlReport,
      documentEpilogue = []
    }
