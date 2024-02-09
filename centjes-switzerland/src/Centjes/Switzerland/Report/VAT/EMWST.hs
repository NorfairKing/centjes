{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.VAT.EMWST
  ( -- * XML Report
    produceXMLReport,
    xmlReportDocument,
    xmlRenderSettings,

    -- ** Types
    XMLReport (..),
    GeneralInformation (..),
    TurnoverComputation (..),
    EffectiveReportingMethod (..),
    TurnoverTaxRate (..),
    OtherFlowOfFunds (..),
  )
where

import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.VAT.Types
import Centjes.Switzerland.XML
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

-- | `VATDeclaration`
--
-- @
-- Das Hauptelement der elektronischen MWST-Deklaration hat den Namen
-- VATDeclaration (root-Element). In der folgenden Tabelle 1 wird die Struktur
-- der elektronischen MWST- Deklaration definiert. Die verwendeten Datentypen
-- sind im Kapitel 5 beschrieben. Um eine bessere Übersicht zu geben, ist die
-- Struktur der gesamten Deklaration in Abbildung 2 darge- stellt.
-- @
--
-- Note that this is a separate type from the Typst 'Input' because the fields
-- of this type are not easy to read from from typst, and don't contain enough
-- info for a good readme. (They're missing the individual deductions, for
-- example.)
data XMLReport = XMLReport
  { -- | `generalInformation`
    --
    -- @
    -- Enthält Angaben zum Steuerpflichtigen und der Abrechnungsperiode
    -- @
    xmlReportGeneralInformation :: !GeneralInformation,
    -- | `turnoverComputation`
    --
    -- @
    -- Enthält Angaben zur Um-satzberechnung
    -- @
    xmlReportTurnoverComputation :: !TurnoverComputation,
    -- | `effectiveReportingMethod`
    --
    -- Ziffer 205 und 3xx bis 4xx
    --
    -- @
    -- Enthält Angaben, welche spezifisch sind für die effektive Methode
    -- (insbesondere die Abzüge)
    -- @
    xmlReportEffectiveReportingMethod :: !EffectiveReportingMethod,
    -- | `payableTax`
    --
    -- Ziffer 500, resp, 510
    --
    -- @
    -- Zu bezahlender Betrag (positives Vorzeichen), resp. Guthaben der steuerpflichtigen Person (negatives Vorzeichen)
    -- @
    xmlReportPayableTax :: !DecimalLiteral,
    -- | `otherFlowsOfFunds`
    --
    -- Ziffer 9xx
    --
    -- @
    -- Enthält alle anderen Mittel-flüsse
    -- @
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
  { -- | `uid`
    --
    -- @
    -- UID des Steuerpflichtigen, wobei im Element uidOrganisationIdCategory
    -- der Präfix (CHE) und im Element uidOrganisationId die neunstellige
    -- Ziffer zu übermitteln ist. Es muss diejenigeUID mit dem Zusatz „MWST“
    -- angegeben werden (= MWST-Nr.; Bsp.: CHE-111.222.333 MWST). Der Zusatz
    -- „MWST“ darf jedoch nicht übermittelt werden.
    -- @
    generalInformationUID :: !UID,
    -- | `organisationName`
    --
    -- @
    -- Firmenname (im UID-System hinterlegter Name)
    -- @
    generalInformationOrganisationName :: !Text,
    -- | `generationTime`
    --
    -- @
    -- Zeitpunkt (Datum und Zeit), an dem das XML generiert wird
    -- @
    generalInformationGenerationTime :: !UTCTime,
    -- | `reportingPeriodFrom`
    --
    -- @
    -- Abrechnungsperiode von (erster Tag der Abrechnungsperiode)
    -- @
    generalInformationReportingPeriodFrom :: !Day,
    -- | `reportingPeriodTill`
    --
    -- @
    -- Abrechnungsperiode bis (letzter Tag der Abrechnungsperiode)
    -- @
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

-- | `eCH-0097:uidStructureType`
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

-- | `turnoverComputationType`
--
-- @
-- Dieser Datentyp enthält Angaben zur Umsatzberechnung (Teil I des
-- MWST-Deklarationsformulars).
-- @
data TurnoverComputation = TurnoverComputation
  { -- | `totalConsideration`
    --
    -- Ziffer 200
    --
    -- @
    -- Total der vereinbarten bzw. vereinnahmten Entgelte, inkl. optierte
    -- Leistungen, Entgelte aus Übertragungen im Meldeverfahren und aus
    -- Leistungen im Ausland.
    -- Hinweis: Bis 31. Dezember 2017 ist der in der Schweiz erzielte Umsatz
    -- anzugeben, ab 1. Januar 2018 der weltweit erzielte Umsatz.
    -- @
    turnoverComputationTotalConsideration :: !DecimalLiteral,
    -- | `suppliesToForeignCountries`
    --
    -- Ziffer 220
    --
    -- @
    -- Leistungen ins Ausland:
    -- Von der Steuer befreite Leistungen (u.a. Exporte), von der Steuer
    -- befreite Leistungen an begünstigte Einrichtungen und Personen
    -- @
    -- TODO reconsider if I should be using this instead
    turnoverComputationSuppliesToForeignCountries :: !(Maybe DecimalLiteral),
    -- | `suppliesAbroad`
    --
    -- Ziffer 221
    --
    -- @
    -- Leistungen im Ausland (Ort der Leistung im Ausland)
    -- @
    turnoverComputationSuppliesAbroad :: !(Maybe DecimalLiteral),
    -- | `transferNotificationProcedure`
    --
    -- Ziffer 225
    --
    -- @
    -- Übertragung im Meldeverfahren
    -- @
    turnoverComputationTransferNotificationProcedure :: !(Maybe DecimalLiteral),
    -- | `suppliesExemptFromTax`
    --
    -- Ziffer 230
    --
    -- @
    -- Von der Steuer ausgenommene Inlandleistungen, für die nicht optiert wird
    -- @
    turnoverComputationSuppliesExemptFromTax :: !(Maybe DecimalLiteral),
    -- | `reductionOfConsideration`
    --
    -- Ziffer 235
    --
    -- @
    -- Entgeltsminderungen wie Skonti, Rabatte usw.
    -- @
    turnoverComputationReductionOfConsideration :: !(Maybe DecimalLiteral),
    -- | `variousDeduction`
    --
    -- Ziffer 280
    --
    -- @
    -- Diverses (z.B. Wert des Bodens, Ankaufspreise Margenbesteuerung)
    -- @
    turnoverComputationVariousDeduction :: !(Maybe DecimalLiteral)
  }
  deriving (Show)

instance ToElement TurnoverComputation where
  toElement TurnoverComputation {..} =
    ech0217Element "turnoverComputation" $
      concat
        [ [NodeElement $ ech0217Element "totalConsideration" [decimalLiteralNode turnoverComputationTotalConsideration]],
          [ NodeElement $ ech0217Element "suppliesToForeignCountries" [decimalLiteralNode dl]
            | dl <- maybeToList turnoverComputationSuppliesToForeignCountries
          ],
          [ NodeElement $ ech0217Element "suppliesAbroad" [decimalLiteralNode dl]
            | dl <- maybeToList turnoverComputationSuppliesAbroad
          ],
          [ NodeElement $ ech0217Element "transferNotificationProcedure" [decimalLiteralNode dl]
            | dl <- maybeToList turnoverComputationTransferNotificationProcedure
          ],
          [ NodeElement $ ech0217Element "suppliesExemptFromTax" [decimalLiteralNode dl]
            | dl <- maybeToList turnoverComputationSuppliesExemptFromTax
          ],
          [ NodeElement $ ech0217Element "reductionOfConsideration" [decimalLiteralNode dl]
            | dl <- maybeToList turnoverComputationReductionOfConsideration
          ],
          [ NodeElement $ ech0217Element "variousDeduction" [decimalLiteralNode dl]
            | dl <- maybeToList turnoverComputationVariousDeduction
          ]
        ]

-- | Ziffer 205 und 3xx bis 4xx
--
-- @
-- Dieser Datentyp enthält alle Elemente, welche spezifisch für die effektive
-- Abrechnungsmethode sind.
-- @
data EffectiveReportingMethod = EffectiveReportingMethod
  { -- | `grossOrNet`
    --
    -- @
    -- Angabe, ob die Umsätze im Element suppliesPerTaxRate und innerhalb
    -- turnoverComputationType Brutto (inkl. MWST) oder Netto angegeben sind.
    -- Die ESTV empfiehlt, nach Möglichkeit die Nettowerte zu übermitteln
    -- (grossOrNet = 1).
    -- 1 = Netto--
    -- 2 = Brutto
    -- @
    effectiveReportingMethodGross :: !Bool,
    -- | `opted`
    --
    -- Ziffer 205
    --
    -- @
    -- Im Element totalConsideration enthaltene entgelte aus von der Steuer
    -- ausgenommenen Leistungen, für welche optiert wird.
    -- @
    effectiveReportingMethodOpted :: !(Maybe DecimalLiteral),
    -- | `suppliesPerTaxRate`
    --
    -- Ziffer 300 bis 379
    --
    -- @
    -- Leistung (Umsatz) pro gesetzlichem Steuersatz; gemäss Angabe im Element
    -- grossOrNet sind alle Leistungen entweder in Brutto oder in Netto
    -- anzugeben.
    -- @
    effectiveReportingMethodSupplies :: ![TurnoverTaxRate],
    -- | `acquisitionTax`
    --
    -- Ziffer 38x
    --
    -- @
    -- Bezugsteuer (Art. 45) Im Element turnoverTaxRateType müssen der Aufwand
    -- und der Steuersatz übermittelt werden; es sind die gesetzlichen
    -- Steuersätze zu verwenden; der Mischsatz ist somit nicht zu übermitteln;
    -- die Bezugsteuer muss immer Netto angegeben werden.
    -- @
    effectiveReportingMethodAcquisitionTax :: ![TurnoverTaxRate],
    -- | `inputTaxMaterialAndServices`
    --
    -- Ziffer 400
    --
    -- @
    -- Vorsteuer auf Material- und Dienstleistungsaufwand
    -- @
    effectiveReportingMethodInputTaxMaterialAndServices :: !(Maybe DecimalLiteral),
    -- | `inputTaxInvestments`
    --
    -- Ziffer 405
    --
    -- @
    -- Vorsteuer auf Investitionen und übrigem Betriebsaufwand
    -- @
    effectiveReportingMethodInputTaxInvestments :: !(Maybe DecimalLiteral),
    -- | `subsequentInputTaxDeduction`
    --
    -- Ziffer 410
    --
    -- @
    -- Einlageentsteuerung (Art. 32)
    -- @
    effectiveReportingMethodSubsequentInputTaxDeduction :: !(Maybe DecimalLiteral),
    -- | `inputTaxCorrections`
    --
    -- Ziffer 415
    --
    -- @
    -- Vorsteuerkorrekturen: gemischte Verwendung (Art. 30), Eigenverbrauch (Art. 31)
    -- @
    effectiveReportingMethodInputTaxCorrections :: !(Maybe DecimalLiteral),
    -- | `inputTaxReductions`
    --
    -- Ziffer 420
    --
    -- @
    -- Vorsteuerkürzungen aufgrund Subventionen usw. (Art. 33 Abs. 2)
    -- @
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

-- | `turnoverTaxRateType`
--
-- @
-- Mittels diesem Datentyp wird der Umsatz pro Steuersatz angegeben. Er wird
-- primär für die Steuerberechnung (Teil II der MWST-Deklarationsformulare)
-- verwendet.
-- @
data TurnoverTaxRate = TurnoverTaxRate
  { -- | `taxRate`
    --
    -- @
    -- Steuersatz in %
    -- @
    turnoverTaxRateRate :: !DecimalLiteral,
    -- | `turnover`
    --
    -- @
    -- Umsatz
    -- @
    turnoverTaxRateTurnover :: !DecimalLiteral
  }
  deriving (Show)

instance ToNodes TurnoverTaxRate where
  toNodes TurnoverTaxRate {..} =
    [ NodeElement $ ech0217Element "taxRate" [decimalLiteralNode turnoverTaxRateRate],
      NodeElement $ ech0217Element "turnover" [decimalLiteralNode turnoverTaxRateTurnover]
    ]

-- | `otherFlowsOfFundsType`
--
-- Ziffer 910 und 910
--
-- @
-- Dieser Datentyp enthält alle anderen Mittelflüsse (Teil III des
-- MWST-Deklarationsformulars).
-- @
data OtherFlowOfFunds = OtherFlowOfFunds
  { -- | `subsidies`
    --
    -- Ziffer 900
    --
    -- @
    -- Subventionen, durch Kurvereine eingenommene Tourismusabgaben,
    -- Entsorgungs- und Wasserwerkbeiträge (Bst. a-c)
    -- @
    otherFlowOfFundsSubsidies :: !DecimalLiteral,
    -- | `donations`
    --
    -- Ziffer 910
    --
    -- @
    -- Spenden, Dividenden, Schadenersatz usw. (Bst. d-l)
    -- @
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

-- | Produce an 'XMLReport' from a 'VATReport' at the given time.
--
-- TODO Put this in a validation instead of a Maybe?
produceXMLReport :: UTCTime -> VATReport ann -> Maybe XMLReport
produceXMLReport generalInformationGenerationTime VATReport {..} = do
  let generalInformationUID =
        UID
          { uidCategory =
              -- @
              -- UID des Steuerpflichtigen, wobei im Element uidOrganisationIdCategory der Präfix (CHE)
              -- @
              "CHE",
            uidId = T.filter (/= '.') vatReportVATId
          }
  let generalInformationOrganisationName = vatReportOrganisationName
  let generalInformationReportingPeriodFrom = periodFirstDay vatReportQuarter
  let generalInformationReportingPeriodTill = periodLastDay vatReportQuarter
  let xmlReportGeneralInformation = GeneralInformation {..}
  let Located _ qf = currencyQuantisationFactor vatReportCHF
      amountLiteral = Amount.toDecimalLiteral qf
      accountLiteral = Account.toDecimalLiteral qf
  turnoverComputationTotalConsideration <- amountLiteral vatReportTotalRevenue
  turnoverComputationSuppliesToForeignCountries <- traverse amountLiteral (unlessZero vatReportTotalExportsRevenue)
  turnoverComputationSuppliesAbroad <- traverse amountLiteral (unlessZero vatReportTotalForeignRevenue)

  -- TODO what is this?
  let turnoverComputationTransferNotificationProcedure = Nothing
  -- TODO what is this?
  let turnoverComputationSuppliesExemptFromTax = Nothing
  -- TODO what is this?
  let turnoverComputationReductionOfConsideration = Nothing
  -- TODO what is this?
  let turnoverComputationVariousDeduction = Nothing
  let xmlReportTurnoverComputation = TurnoverComputation {..}
  standard2023TurnoverLiteral <- amountLiteral vatReportDomesticRevenue2023
  standard2024TurnoverLiteral <- amountLiteral vatReportDomesticRevenue2024
  -- TODO what is this?
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
  effectiveReportingMethodInputTaxInvestments <- traverse amountLiteral (unlessZero vatReportPaidVAT)
  -- TODO what's this?
  let effectiveReportingMethodSubsequentInputTaxDeduction = Nothing
  let effectiveReportingMethodInputTaxCorrections = Nothing
  let effectiveReportingMethodInputTaxReductions = Nothing
  let xmlReportEffectiveReportingMethod = EffectiveReportingMethod {..}
  xmlReportPayableTax <- accountLiteral vatReportPayable
  -- TODO gather donations and subsidies
  let xmlReportOtherFlowOfFunds = Nothing
  pure XMLReport {..}

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
