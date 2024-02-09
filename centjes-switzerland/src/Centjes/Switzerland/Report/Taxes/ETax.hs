{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Version (showVersion)
import qualified Paths_centjes_switzerland as CentjesSwitzerland (version)
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
              ("xmlns", "http://www.ech.ch/xmlns/eCH-0119/4"),
              (xsiName "schemaLocation", "http://www.ech.ch/xmlns/eCH-0119/4 eCH-0119-4-0_draft.xsd")
            ],
        elementNodes =
          [ NodeElement $ toElement xmlReportHeader,
            NodeElement $ toElement xmlReportContent
          ]
      }

data Header = Header
  { headerAttachments :: ![Attachment],
    headerCantonExtension :: !(),
    headerTransactionNumber :: !(Maybe Text),
    headerTransactionDate :: !(Maybe LocalTime),
    headerTaxPeriod :: !Year,
    headerPeriodFrom :: !(Maybe Day),
    headerPeriodTo :: !(Maybe Day),
    headerCanton :: !(Maybe CantonAbbreviation)
  }
  deriving (Show)

instance ToElement Header where
  toElement Header {..} =
    ech0119Element "header" $
      concat
        [ map (NodeElement . toElement) headerAttachments,
          -- cantonExtension
          [ NodeElement $ ech0119Element "transactionNumber" [NodeContent tn]
            | tn <- maybeToList headerTransactionNumber
          ],
          [ NodeElement $ ech0119Element "transactionDate" [NodeContent $ T.pack $ formatTime defaultTimeLocale "%FT%T" lt]
            | lt <- maybeToList headerTransactionDate
          ],
          [NodeElement $ ech0119Element "taxPeriod" [NodeContent $ T.pack $ show headerTaxPeriod]],
          [ NodeElement $ ech0119Element "periodFrom" [NodeContent $ T.pack $ formatTime defaultTimeLocale "%F" d]
            | d <- maybeToList headerPeriodFrom
          ],
          [ NodeElement $ ech0119Element "periodTo" [NodeContent $ T.pack $ formatTime defaultTimeLocale "%F" d]
            | d <- maybeToList headerPeriodTo
          ],
          [ NodeElement $ ech0119Element "canton" [NodeContent $ renderCantonAbbreviation ca]
            | ca <- maybeToList headerCanton
          ],
          [ NodeElement $ ech0119Element "source" [NodeContent "0"],
            NodeElement $ ech0119Element "sourceDescription" [NodeContent $ T.pack $ "centjes-switzerland-" <> showVersion CentjesSwitzerland.version]
          ]
        ]

-- TODO Attachment
data Attachment = Attachment
  deriving (Show)

instance ToElement Attachment where
  toElement Attachment {} = ech0119Element "attachment" []

-- TODO other cantons if we support them
data CantonAbbreviation = CantonAbbreviationZuerich
  deriving (Show)

renderCantonAbbreviation :: CantonAbbreviation -> Text
renderCantonAbbreviation = \case
  CantonAbbreviationZuerich -> "ZH"

-- TODO Content
data Content = Content
  { contentMainForm :: !(Maybe MainForm)
  }
  -- TODO mainForm
  -- TODO listOfSecurities
  -- TODO listOfLiabilities
  -- TODO qualifiedInvestmentsPrivate
  -- TODO qualifiedInvestmentsBusiness
  -- TODO jobExpenses
  -- TODO jobOrientedFurtherEducationCost
  -- TODO insurancePremiums
  -- TODO diseaseAndAccidentExpenses
  -- TODO handicapExpenses
  -- TODO cantonExtension
  deriving (Show)

instance ToElement Content where
  toElement Content {..} =
    ech0119Element "content" $
      [ NodeElement $ toElement mf
        | mf <- maybeToList contentMainForm
      ]

data MainForm = MainForm
  { mainFormPersonDataPartner1 :: !PersonDataPartner1
  -- TODO representativePerson
  -- TODO personDataPartner1
  -- TODO personDataPartner2
  -- TODO childData
  -- TODO disabledPersonSupport
  -- TODO revenue
  -- TODO deduction
  -- TODO revenueCalculation
  -- TODO asset
  -- TODO benefit
  -- TODO attachedForms
  -- TODO cantonExtension
  -- TODO lastTaxDeclaration
  }
  deriving (Show)

instance ToElement MainForm where
  toElement MainForm {..} =
    ech0119Element
      "mainForm"
      [ NodeElement $ toElement mainFormPersonDataPartner1
      ]

data PersonDataPartner1 = PersonDataPartner1
  { personDataPartner1PartnerPersonIdentification :: !PartnerPersonIdentification
  -- TODO adressinformation
  -- TODO cantonExtension
  -- TODO maritalStatusTax
  -- TODO religion
  -- TODO job
  -- TODO employer
  -- TODO placeOfWork
  -- TODO phoneNumberPrivate
  -- TODO phoneNumberBusiness
  -- TODO paymentPension
  -- TODO taxMunicipality
  }
  deriving (Show)

instance ToElement PersonDataPartner1 where
  toElement PersonDataPartner1 {..} =
    ech0119Element
      "personDataPartner1"
      [ NodeElement $ toElement personDataPartner1PartnerPersonIdentification
      ]

data PartnerPersonIdentification = PartnerPersonIdentification
  { -- TODO cantonExtension
    partnerPersonIdentificationOfficialName :: !Text,
    partnerPersonIdentificationFirstName :: !Text,
    -- TODO partnerPersonIdentificationSex
    -- TODO partnerPersonIdentificationDateOfBirth
    partnerPersonIdentificationVn :: !Text
    -- TODO partnerPersonIdentificationOtherPersonID
  }
  deriving (Show)

instance ToElement PartnerPersonIdentification where
  toElement PartnerPersonIdentification {..} =
    ech0119Element
      "partnerPersonIdentification"
      [ NodeElement $ ech0119Element "officialName" [NodeContent partnerPersonIdentificationOfficialName],
        NodeElement $ ech0119Element "firstName" [NodeContent partnerPersonIdentificationFirstName],
        NodeElement $ ech0119Element "vn" [NodeContent partnerPersonIdentificationVn]
      ]

-- | Produce an 'XMLReport' from a 'TaxesReport' at the given time.
--
-- TODO Put this in a validation instead of a Maybe?
produceXMLReport :: UTCTime -> TaxesReport ann -> Maybe XMLReport
produceXMLReport _ TaxesReport {..} = do
  let headerAttachments = []
  let headerCantonExtension = ()
  let headerTransactionNumber = Nothing
  let headerTransactionDate = Nothing
  let headerTaxPeriod = taxesReportYear
  let headerPeriodFrom = Just $ fromGregorian taxesReportYear 1 1
  let headerPeriodTo = Just $ fromGregorian taxesReportYear 12 31
  let headerCanton = Nothing
  let xmlReportHeader = Header {..}
  let partnerPersonIdentificationOfficialName = taxesReportLastName
  let partnerPersonIdentificationFirstName = taxesReportFirstName
  let partnerPersonIdentificationVn = taxesReportInsuredPersonNumber
  let personDataPartner1PartnerPersonIdentification = PartnerPersonIdentification {..}
  let mainFormPersonDataPartner1 = PersonDataPartner1 {..}
  let contentMainForm = Just MainForm {..}
  let xmlReportContent = Content {..}
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
