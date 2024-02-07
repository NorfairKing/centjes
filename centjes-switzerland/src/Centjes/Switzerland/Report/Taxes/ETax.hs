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

import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.Taxes.Types
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Data.Version
import qualified Money.Account as Account
import qualified Money.Amount as Amount
import qualified Money.Amount as Money (Amount)
import Numeric.DecimalLiteral (DecimalLiteral)
import qualified Numeric.DecimalLiteral as DecimalLiteral
import qualified Paths_centjes_switzerland as CentjesSwitzerland (version)
import Text.XML as XML

class ToElement a where
  toElement :: a -> Element

class ToNodes a where
  toNodes :: a -> [XML.Node]

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

ech0119Element :: Text -> [XML.Node] -> XML.Element
ech0119Element name = xmlElement (ech0119Name name)

xmlElement :: XML.Name -> [XML.Node] -> XML.Element
xmlElement elementName elementNodes =
  let elementAttributes = M.empty
   in XML.Element {..}

decimalLiteralNode :: DecimalLiteral -> XML.Node
decimalLiteralNode =
  XML.NodeContent
    . T.pack
    . DecimalLiteral.format
    . DecimalLiteral.setSignOptional

-- | Produce an 'XMLReport' from a 'TaxesReport' at the given time.
--
-- TODO Put this in a validation instead of a Maybe?
produceXMLReport :: UTCTime -> TaxesReport ann -> Maybe XMLReport
produceXMLReport generationTime TaxesReport {..} = do
  pure XMLReport {}

unlessZero :: Money.Amount -> Maybe Money.Amount
unlessZero a = if a == Amount.zero then Nothing else Just a

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

xsiName :: Text -> XML.Name
xsiName = xmlName "http://www.w3.org/2001/XMLSchema-instance" "xsi"

ech0119Name :: Text -> XML.Name
ech0119Name = xmlName "http://www.ech.ch/xmlns/eCH-0119/3" "eCH-0119"

xmlName :: Text -> Text -> Text -> XML.Name
xmlName namespace prefix nameLocalName =
  let nameNamespace = Just namespace
      namePrefix = Just prefix
   in XML.Name {..}
