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
import Data.Time
import Text.XML as XML

-- | `taxDeclaration`
data XMLReport = XMLReport
  { xmlReportHeader :: Header,
    xmlReportContent :: Content
  }
  deriving (Show)

instance ToElement XMLReport where
  toElement XMLReport {..} =
    XML.Element
      { elementName = "message",
        elementAttributes =
          M.fromList
            [ ("minorVersion", "0"),
              (xsiName "schemaLocation", "http://www.ech.ch/xmlns/eCH-0119/4 eCH-0119-4-0_draft.xsd")
            ],
        elementNodes =
          [ NodeElement $ toElement xmlReportHeader,
            NodeElement $ toElement xmlReportContent
          ]
      }

data Header = Header
  deriving (Show)

instance ToElement Header where
  toElement Header {} = ech0119Element "header" []

data Content = Content
  deriving (Show)

instance ToElement Content where
  toElement Content {} = ech0119Element "content" []

-- | Produce an 'XMLReport' from a 'TaxesReport' at the given time.
--
-- TODO Put this in a validation instead of a Maybe?
produceXMLReport :: UTCTime -> TaxesReport ann -> Maybe XMLReport
produceXMLReport _ TaxesReport {} = do
  let xmlReportHeader = Header
  let xmlReportContent = Content
  pure XMLReport {..}

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
