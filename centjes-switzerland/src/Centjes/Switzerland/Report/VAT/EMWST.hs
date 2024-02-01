{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.VAT.EMWST
  ( XMLReport (..),
    xmlReportDocument,
    xmlRenderSettings,
    produceXMLReport,
  )
where

import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.VAT.Types
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Data.Version
import qualified Money.Account as Account
import qualified Money.Amount as Amount
import Numeric.DecimalLiteral (DecimalLiteral)
import qualified Numeric.DecimalLiteral as DecimalLiteral
import qualified Paths_centjes_switzerland as CentjesSwitzerland (version)
import Text.XML as XML

class ToElement a where
  toElement :: a -> Element

class ToNodes a where
  toNodes :: a -> [XML.Node]

-- Rename the types to the names in the specifications
data XMLReport = XMLReport
  { xmlReportGeneralInformation :: !GeneralInformation,
    xmlReportTurnoverComputation :: !TurnoverComputation,
    xmlReportEffectiveReportingMethod :: !EffectiveReportingMethod,
    xmlReportPayableTax :: !DecimalLiteral,
    xmlReportOtherFlowOfFunds :: !(Maybe OtherFlowOfFunds)
  }
  deriving (Show)

instance ToElement XMLReport where
  toElement XMLReport {..} =
    XML.Element
      { elementName = ech0217Name "VATDeclaration",
        elementAttributes =
          M.fromList
            [ (xsiName "schemaLocation", "http://www.ech.ch/xmlns/eCH-0217/1 eCH-0217-1-0.xsd")
            ],
        elementNodes =
          concat
            [ [ NodeElement $ toElement xmlReportGeneralInformation,
                NodeElement $ toElement xmlReportTurnoverComputation,
                NodeElement $ toElement xmlReportEffectiveReportingMethod,
                NodeElement $ ech0217Element "payableTax" [decimalLiteralNode xmlReportPayableTax]
              ],
              [ NodeElement $ toElement o
                | o <- maybeToList xmlReportOtherFlowOfFunds
              ]
            ]
      }

data GeneralInformation = GeneralInformation
  { generalInformationUID :: !UID,
    generalInformationOrganisationName :: !Text,
    generalInformationGenerationTime :: !UTCTime,
    generalInformationReportingPeriodFrom :: !Day,
    generalInformationReportingPeriodTill :: !Day
  }
  deriving (Show)

instance ToElement GeneralInformation where
  toElement GeneralInformation {..} =
    ech0217Element
      "generalInformation"
      [ NodeElement $ toElement generalInformationUID,
        NodeElement $ ech0217Element "organisationName" [NodeContent generalInformationOrganisationName],
        NodeElement $ ech0217Element "generationTime" [NodeContent $ T.pack $ iso8601Show generalInformationGenerationTime],
        NodeElement $ ech0217Element "reportingPeriodFrom" [NodeContent $ T.pack $ iso8601Show generalInformationReportingPeriodFrom],
        NodeElement $ ech0217Element "reportingPeriodTill" [NodeContent $ T.pack $ iso8601Show generalInformationReportingPeriodTill],
        -- We use the "first submission" type
        NodeElement $ ech0217Element "typeOfSubmission" [NodeContent "1"],
        -- We use received amounts, not agreed amounts
        NodeElement $ ech0217Element "formOfReporting" [NodeContent "2"],
        -- We use the generation time as the reference id
        NodeElement $ ech0217Element "businessReferenceId" [NodeContent $ T.pack $ iso8601Show generalInformationGenerationTime],
        NodeElement $
          ech0217Element
            "sendingApplication"
            [ NodeElement $ ech0058Element "manufacturer" [NodeContent "CS-SYD"],
              NodeElement $ ech0058Element "product" [NodeContent "centjes"],
              NodeElement $ ech0058Element "productVersion" [NodeContent $ T.pack $ showVersion CentjesSwitzerland.version]
            ]
      ]

data UID = UID
  { uidCategory :: !Text,
    uidId :: !Text
  }
  deriving (Show)

instance ToElement UID where
  toElement UID {..} =
    ech0217Element
      "uid"
      [ NodeElement $ ech0097Element "uidOrganisationIdCategorie" [NodeContent uidCategory],
        NodeElement $ ech0097Element "uidOrganisationId" [NodeContent uidId]
      ]

data TurnoverComputation = TurnoverComputation
  { turnoverComputationTotalConsideration :: !DecimalLiteral,
    turnoverComputationSuppliesAbroad :: !DecimalLiteral
  }
  deriving (Show)

instance ToElement TurnoverComputation where
  toElement TurnoverComputation {..} =
    ech0217Element
      "turnoverComputation"
      [ NodeElement $ ech0217Element "totalConsideration" [decimalLiteralNode turnoverComputationTotalConsideration],
        NodeElement $ ech0217Element "suppliesAbroad" [decimalLiteralNode turnoverComputationSuppliesAbroad]
      ]

data EffectiveReportingMethod = EffectiveReportingMethod
  { effectiveReportingMethodGross :: !Bool,
    effectiveReportingMethodOpted :: !(Maybe DecimalLiteral),
    effectiveReportingMethodSupplies :: ![TurnoverTaxRate],
    effectiveReportingMethodAcquisitionTax :: ![TurnoverTaxRate],
    effectiveReportingMethodInputTaxMaterialAndServices :: !(Maybe DecimalLiteral),
    effectiveReportingMethodInputTaxInvestments :: !(Maybe DecimalLiteral),
    effectiveReportingMethodSubsequentInputTaxDeduction :: !(Maybe DecimalLiteral),
    effectiveReportingMethodInputTaxCorrections :: !(Maybe DecimalLiteral),
    effectiveReportingMethodInputTaxReductions :: !(Maybe DecimalLiteral)
  }
  deriving (Show)

instance ToElement EffectiveReportingMethod where
  toElement EffectiveReportingMethod {..} =
    ech0217Element
      "effectiveReportingMethod"
      $ concat
        [ [NodeElement $ ech0217Element "grossOrNet" [NodeContent "2"]],
          [NodeElement $ ech0217Element "opted" [decimalLiteralNode dl] | dl <- maybeToList effectiveReportingMethodOpted],
          map
            (NodeElement . ech0217Element "suppliesPerTaxRate" . toNodes)
            effectiveReportingMethodSupplies,
          map
            (NodeElement . ech0217Element "acquisitionTax" . toNodes)
            effectiveReportingMethodAcquisitionTax,
          [NodeElement $ ech0217Element "inputTaxMaterialAndServices" [decimalLiteralNode dl] | dl <- maybeToList effectiveReportingMethodInputTaxMaterialAndServices],
          [NodeElement $ ech0217Element "inputTaxInvestments" [decimalLiteralNode dl] | dl <- maybeToList effectiveReportingMethodInputTaxInvestments],
          [NodeElement $ ech0217Element "subsequentInputTaxDeduction" [decimalLiteralNode dl] | dl <- maybeToList effectiveReportingMethodSubsequentInputTaxDeduction],
          [NodeElement $ ech0217Element "inputTaxCorrections" [decimalLiteralNode dl] | dl <- maybeToList effectiveReportingMethodInputTaxCorrections],
          [NodeElement $ ech0217Element "inputTaxReductions" [decimalLiteralNode dl] | dl <- maybeToList effectiveReportingMethodInputTaxReductions]
        ]

data TurnoverTaxRate = TurnoverTaxRate
  { turnoverTaxRateRate :: !DecimalLiteral,
    turnoverTaxRateTurnover :: !DecimalLiteral
  }
  deriving (Show)

instance ToNodes TurnoverTaxRate where
  toNodes TurnoverTaxRate {..} =
    [ NodeElement $ ech0217Element "taxRate" [decimalLiteralNode turnoverTaxRateRate],
      NodeElement $ ech0217Element "turnover" [decimalLiteralNode turnoverTaxRateTurnover]
    ]

data OtherFlowOfFunds = OtherFlowOfFunds
  { otherFlowOfFundsSubsidies :: !DecimalLiteral,
    otherFlowOfFundsDonations :: !DecimalLiteral
  }
  deriving (Show)

instance ToElement OtherFlowOfFunds where
  toElement OtherFlowOfFunds {..} =
    ech0217Element
      "otherFlowOfFunds"
      [ NodeElement $ ech0217Element "subsidies" [decimalLiteralNode otherFlowOfFundsSubsidies],
        NodeElement $ ech0217Element "donations" [decimalLiteralNode otherFlowOfFundsDonations]
      ]

ech0058Element :: Text -> [XML.Node] -> XML.Element
ech0058Element name = xmlElement (ech0058Name name)

ech0097Element :: Text -> [XML.Node] -> XML.Element
ech0097Element name = xmlElement (ech0097Name name)

ech0217Element :: Text -> [XML.Node] -> XML.Element
ech0217Element name = xmlElement (ech0217Name name)

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

-- TODO Put this in a validation instead of a Maybe?
produceXMLReport :: UTCTime -> VATReport ann -> Maybe XMLReport
produceXMLReport generalInformationGenerationTime VATReport {..} = do
  let generalInformationUID =
        -- TODO fill in the real data
        UID
          { uidCategory = "CHE",
            uidId = "5"
          }
  let generalInformationOrganisationName = "CS Kerckhove"
  let generalInformationReportingPeriodFrom = periodFirstDay vatReportQuarter
  let generalInformationReportingPeriodTill = periodLastDay vatReportQuarter
  let xmlReportGeneralInformation = GeneralInformation {..}
  let Located _ qf = currencyQuantisationFactor vatReportCHF
      amountLiteral = Amount.toDecimalLiteral qf
      accountLiteral = Account.toDecimalLiteral qf
  turnoverComputationTotalConsideration <- amountLiteral vatReportTotalRevenue
  turnoverComputationSuppliesAbroad <- amountLiteral vatReportTotalRevenue
  let xmlReportTurnoverComputation = TurnoverComputation {..}
  standard2023TurnoverLiteral <- amountLiteral vatReportDomesticRevenue2023
  standard2024TurnoverLiteral <- amountLiteral vatReportDomesticRevenue2024
  let effectiveReportingMethodGross = True
  -- TODO what is this?
  let effectiveReportingMethodOpted = Nothing
  let effectiveReportingMethodSupplies =
        [ TurnoverTaxRate
            { -- TODO generate this decimal literal from the same TaxRate type that produced it
              turnoverTaxRateRate = "7.7",
              turnoverTaxRateTurnover = standard2023TurnoverLiteral
            },
          TurnoverTaxRate
            { -- TODO generate this decimal literal from the same TaxRate type that produced it
              turnoverTaxRateRate = "8.1",
              turnoverTaxRateTurnover = standard2024TurnoverLiteral
            }
        ]
  -- TODO what's this?
  let effectiveReportingMethodAcquisitionTax = []
  -- TODO what's this?
  let effectiveReportingMethodInputTaxMaterialAndServices = Nothing
  effectiveReportingMethodInputTaxInvestments <- Just <$> amountLiteral vatReportPaidVAT
  -- TODO what's this?
  let effectiveReportingMethodSubsequentInputTaxDeduction = Nothing
  let effectiveReportingMethodInputTaxCorrections = Nothing
  let effectiveReportingMethodInputTaxReductions = Nothing
  let xmlReportEffectiveReportingMethod = EffectiveReportingMethod {..}
  xmlReportPayableTax <- accountLiteral vatReportPayable
  -- TODO gather donations and subsidies
  let xmlReportOtherFlowOfFunds = Nothing
  pure XMLReport {..}

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

xmlRenderSettings :: XML.RenderSettings
xmlRenderSettings =
  def
    { rsXMLDeclaration = True,
      rsNamespaces =
        [ ("eCH-0058", "http://www.ech.ch/xmlns/eCH-0058/5"),
          ("eCH-0097", "http://www.ech.ch/xmlns/eCH-0097/3"),
          ("eCH-0217", "http://www.ech.ch/xmlns/eCH-0217/1"),
          ("xsi", "http://www.w3.org/2001/XMLSchema-instance")
        ]
    }

xsiName :: Text -> XML.Name
xsiName = xmlName "http://www.w3.org/2001/XMLSchema-instance" "xsi"

ech0058Name :: Text -> XML.Name
ech0058Name = xmlName "http://www.ech.ch/xmlns/eCH-0058/5" "eCH-0058"

ech0097Name :: Text -> XML.Name
ech0097Name = xmlName "http://www.ech.ch/xmlns/eCH-0097/3" "eCH-0097"

ech0217Name :: Text -> XML.Name
ech0217Name = xmlName "http://www.ech.ch/xmlns/eCH-0217/1" "eCH-0217"

xmlName :: Text -> Text -> Text -> XML.Name
xmlName namespace prefix nameLocalName =
  let nameNamespace = Just namespace
      namePrefix = Just prefix
   in XML.Name {..}
