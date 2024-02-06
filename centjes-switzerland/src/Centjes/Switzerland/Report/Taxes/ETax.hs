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
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time
import Text.XML as XML

class ToElement a where
  toElement :: a -> Element

-- | `taxDeclaration`
data XMLReport = XMLReport
  deriving (Show)

instance ToElement XMLReport where
  toElement XMLReport {} =
    XML.Element
      { elementName = ech0119Name "taxDeclaration",
        elementAttributes = M.empty,
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

-- | Render with the four relevant namespaces and an XML declaration.
xmlRenderSettings :: XML.RenderSettings
xmlRenderSettings =
  def
    { rsXMLDeclaration = True,
      rsNamespaces =
        [ ("eCH-0119", "http://www.ech.ch/xmlns/eCH-0119/3"),
          ("xsi", "http://www.w3.org/2001/XMLSchema-instance")
        ]
    }

ech0119Name :: Text -> XML.Name
ech0119Name = xmlName "http://www.ech.ch/xmlns/eCH-0119/3" "eCH-0119"

xmlName :: Text -> Text -> Text -> XML.Name
xmlName namespace prefix nameLocalName =
  let nameNamespace = Just namespace
      namePrefix = Just prefix
   in XML.Name {..}
