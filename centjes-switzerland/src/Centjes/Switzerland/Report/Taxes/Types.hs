{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.Taxes.Types
  ( TaxesInput (..),
    TaxesReport (..),
    AssetAccount (..),
    TaxesError (..),
  )
where

import Centjes.Convert
import Centjes.Format
import Centjes.Ledger
import Centjes.Location
import Centjes.Module
import Centjes.Report.Balance
import qualified Centjes.Tag as Tag
import Centjes.Validation
import Data.List
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Validity.Time ()
import Error.Diagnose
import GHC.Generics (Generic (..))
import qualified Money.Account as Money (Account)
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.MultiAccount as Money (MultiAccount)
import Money.QuantisationFactor as Money (QuantisationFactor (..))
import OptEnvConf
import Path

-- | The settings we need to produce a 'TaxesReport'
data TaxesInput = TaxesInput
  { taxesInputLastName :: !Text,
    taxesInputFirstName :: !Text,
    taxesInputYear :: !Year,
    taxesInputTagUndeclared :: !Tag,
    taxesInputTagDeductible :: !Tag,
    taxesInputTagNotDeductible :: !Tag,
    taxesInputTagTaxDeductible :: !Tag,
    taxesInputTagNotTaxDeductible :: !Tag,
    taxesInputInsuredPersonNumber :: !Text
  }
  deriving (Show, Generic)

instance Validity TaxesInput

instance HasParser TaxesInput where
  settingsParser = parseTaxesInput

{-# ANN parseTaxesInput ("NOCOVER" :: String) #-}
parseTaxesInput :: Parser TaxesInput
parseTaxesInput = do
  taxesInputLastName <-
    setting
      [ help "your first name",
        conf "first-name"
      ]
  taxesInputFirstName <-
    setting
      [ help "your last name",
        conf "last-name"
      ]
  taxesInputYear <-
    choice
      [ setting
          [ help "the year to produce the report for",
            conf "year"
          ],
        runIO $ (\d -> let (y, _, _) = toGregorian d in y) . utctDay <$> getCurrentTime
      ]
  taxesInputTagUndeclared <-
    setting
      [ help "tag to use for undeclared asset accounts",
        reader $ eitherReader Tag.fromString,
        conf "tag-undeclared",
        value "undeclared"
      ]
  taxesInputTagDeductible <-
    setting
      [ help "tag to use for deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-deductible",
        value "deductible"
      ]
  taxesInputTagNotDeductible <-
    setting
      [ help "tag to use for non-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-deductible",
        value "not-deductible"
      ]
  taxesInputTagTaxDeductible <-
    setting
      [ help "tag to use for tax-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-tax-deductible",
        value "tax-deductible"
      ]
  taxesInputTagNotTaxDeductible <-
    setting
      [ help "tag to use for non-tax-deductible purchases",
        reader $ eitherReader Tag.fromString,
        conf "tag-not-tax-deductible",
        value "not-tax-deductible"
      ]
  taxesInputInsuredPersonNumber <-
    setting
      [ help "The AHV identifier.",
        conf "ahv-id",
        example "7461111222233"
      ]
  pure TaxesInput {..}

-- | The information we need to produce Taxes reports like the pdfs, zip files,
-- or xml files.
data TaxesReport ann = TaxesReport
  { taxesReportLastName :: !Text,
    taxesReportFirstName :: !Text,
    taxesReportYear :: !Year,
    taxesReportInsuredPersonNumber :: !Text,
    taxesReportCHF :: !(Currency ann),
    taxesReportConversionRates :: !(Map (Currency ann) Money.ConversionRate),
    taxesReportAssetAccounts :: ![AssetAccount ann]
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TaxesReport ann) where
  validate vr@TaxesReport {} =
    mconcat
      [ genericValidate vr
      ]

data AssetAccount ann = AssetAccount
  { assetAccountName :: !AccountName,
    assetAccountBalance :: !(Money.MultiAccount (Currency ann)),
    assetAccountConvertedBalance :: !Money.Account,
    assetAccountAttachments :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (AssetAccount ann)

data TaxesError ann
  = TaxesErrorNoCHF
  | TaxesErrorWrongCHF !(GenLocated ann Money.QuantisationFactor)
  | TaxesErrorConvertError !(ConvertError ann)
  | TaxesErrorBalanceError !(BalanceError ann)
  | TaxesErrorAssetAccountWithoutEvidence !(GenLocated ann AccountName)
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (TaxesError ann)

instance ToReport (TaxesError SourceSpan) where
  toReport = \case
    TaxesErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    TaxesErrorWrongCHF (Located cdl _) ->
      Err
        Nothing
        "Incompatible CHF defined"
        [(toDiagnosePosition cdl, This "This currency declaration must use 0.01")]
        []
    TaxesErrorConvertError ce -> toReport ce
    TaxesErrorBalanceError be -> toReport be
    TaxesErrorAssetAccountWithoutEvidence (Located anl an) ->
      Err
        Nothing
        "Asset account without evidence"
        [ (toDiagnosePosition anl, This "This account declaration has neither evidence nor is tagged as undeclared")
        ]
        [ Hint $
            unlines'
              [ "You can tag this account as undeclared:",
                T.unpack $
                  T.strip $
                    formatDeclaration $
                      DeclarationAccount $
                        noLoc $
                          AccountDeclaration
                            { accountDeclarationName = noLoc an,
                              accountDeclarationType = Nothing,
                              accountDeclarationExtras = [noLoc $ AccountExtraTag $ noLoc $ ExtraTag $ noLoc "undeclared"]
                            }
              ]
        ]

unlines' :: [String] -> String
unlines' = intercalate "\n"
