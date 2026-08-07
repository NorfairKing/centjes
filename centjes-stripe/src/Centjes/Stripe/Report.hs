{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading Stripe's financial report CSVs.
--
-- Every field is looked up by column name, never by position, and a column that is
-- asked for and missing is an error naming it.  That matters more here than it
-- looks: the importer asks for its columns explicitly when it creates the run, so a
-- name Stripe has changed shows up as a named failure rather than as a column of
-- numbers read as something else.
--
-- Report CSVs express money in /major/ units as a decimal, unlike the rest of the
-- Stripe API which uses minor units as an integer.  Real responses have been seen
-- with two decimal places and with eighteen, so amounts go through a decimal
-- literal rather than any fixed-width parse, and a value the ledger
-- currency cannot represent exactly is an error rather than a rounding.
module Centjes.Stripe.Report
  ( ReportTypeId (..),
    ReportType (..),
    reportTypeCoversThrough,
    ReportKind (..),
    allReportKinds,
    reportKindTypeId,
    reportKindColumns,
    reportKindAttachmentPattern,
    ReportColumn (..),
    ReportTable (..),
    ReportRow (..),
    parseReportTable,
    ReportError (..),
    renderReportError,
    requireColumn,
    requireAmount,
    requireRational,
    columnOrEmpty,
  )
where

import Autodocodec
import Centjes.Stripe.Timestamp
import qualified Data.ByteString.Lazy as LB
import Data.Csv (NamedRecord)
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.Text ()
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor (QuantisationFactor (..))
import Numeric.DecimalLiteral (DecimalLiteral)
import qualified Numeric.DecimalLiteral as DecimalLiteral

-- | One of the reports this importer reads.
--
-- Enumerated rather than named by string at each use, so that adding a report forces
-- every place that has to say something about one to be reconsidered: which columns to
-- ask for, and what the saved CSV is called.
data ReportKind
  = -- | Sales and tax collected, split by country and rate.
    ReportKindTax
  | -- | What the balance moved by, per reporting category.
    ReportKindActivity
  | -- | Stripe's own fees, and the VAT on them.
    ReportKindFees
  | -- | The balance at the start and end of the month.
    ReportKindBalance

allReportKinds :: [ReportKind]
allReportKinds = [ReportKindTax, ReportKindActivity, ReportKindFees, ReportKindBalance]

reportKindTypeId :: ReportKind -> ReportTypeId
reportKindTypeId = \case
  ReportKindTax -> ReportTypeId "tax.summarized_export.1"
  ReportKindActivity -> ReportTypeId "balance_change_from_activity.summary.2"
  ReportKindFees -> ReportTypeId "all_fees.balance_transaction_created.summary.2"
  ReportKindBalance -> ReportTypeId "balance.summary.1"

-- | The columns to ask Stripe for.
--
-- Asked for explicitly so that a column Stripe has renamed fails by name when the
-- report is read, rather than being read as something else.
reportKindColumns :: ReportKind -> [ReportColumn]
reportKindColumns =
  map ReportColumn . \case
    ReportKindTax ->
      [ "country_code",
        "jurisdiction_level",
        "jurisdiction_name",
        "tax_rate",
        "transaction_currency",
        "total_sales",
        "total_tax_collected",
        "total_taxable_sales",
        "total_nontaxable_sales",
        "total_sales_refunded",
        "total_tax_refunded"
      ]
    ReportKindActivity -> ["reporting_category", "currency", "gross", "fee", "net"]
    ReportKindFees -> ["suite", "product", "feature_name", "amount", "tax", "currency"]
    ReportKindBalance -> ["category", "description", "net_amount", "currency"]

-- | 'formatTime' pattern for the CSV a month's report is saved as.
--
-- The importer saves what Stripe sent and attaches it to the transactions computed from
-- it, so this name is both what is written to disk and what the ledger refers to.  A
-- pattern rather than a fixed name so that the date the file is filed under is the last
-- day of the month it covers, matching how the other monthly documents are named.
reportKindAttachmentPattern :: ReportKind -> String
reportKindAttachmentPattern = \case
  ReportKindTax -> "%Y-%m-%d_stripe-tax-summary-%Y-%m.csv"
  ReportKindActivity -> "%Y-%m-%d_stripe-activity-summary-%Y-%m.csv"
  ReportKindFees -> "%Y-%m-%d_stripe-fee-summary-%Y-%m.csv"
  ReportKindBalance -> "%Y-%m-%d_stripe-balance-summary-%Y-%m.csv"

-- | The name of a column in a report CSV.
--
-- A newtype so that a column name cannot be confused with a field's value, which is
-- also text.
newtype ReportColumn = ReportColumn {unReportColumn :: Text}
  deriving stock (Show, Eq)

-- | One row of a report, addressable by column name.
newtype ReportRow = ReportRow {unReportRow :: NamedRecord}

-- | A whole report CSV.
data ReportTable = ReportTable
  { reportTableColumns :: ![ReportColumn],
    reportTableRows :: ![ReportRow]
  }

data ReportError
  = -- | The CSV would not parse at all.
    ReportErrorUnparseable !String
  | ReportErrorNoSuchColumn !ReportColumn ![ReportColumn]
  | ReportErrorNotUtf8 !ReportColumn
  | ReportErrorNotADecimal !ReportColumn !Text
  | -- | A decimal the ledger's currency cannot hold exactly.
    ReportErrorNotRepresentable !ReportColumn !Text !QuantisationFactor
  deriving stock (Show, Eq)

renderReportError :: ReportError -> String
renderReportError = \case
  ReportErrorUnparseable reason ->
    unwords ["The report is not a CSV this importer can read:", reason]
  ReportErrorNoSuchColumn column available ->
    unlines
      [ unwords ["The report has no column named", show (unReportColumn column) <> "."],
        unwords ["It has:", show (map unReportColumn available)],
        "The importer asks Stripe for the columns it wants by name, so this means Stripe has renamed or dropped one.",
        "It can also mean this is a saved copy from before the importer asked for that column, in which case deleting the file lets the next run fetch it again."
      ]
  ReportErrorNotUtf8 column ->
    unwords ["The column", show (unReportColumn column), "is not valid UTF-8."]
  ReportErrorNotADecimal column raw ->
    unwords ["The column", show (unReportColumn column), "holds", show raw <> ",", "which is not a decimal number."]
  ReportErrorNotRepresentable column raw qf ->
    unlines
      [ unwords
          [ "The column",
            show (unReportColumn column),
            "holds",
            show raw <> ",",
            "which cannot be expressed exactly with quantisation factor",
            show (unQuantisationFactor qf) <> "."
          ],
        "Rounding money silently is how a reconciliation stops meaning anything, so this is an error."
      ]

-- | Read a report CSV, header and all.
parseReportTable :: LB.ByteString -> Either ReportError ReportTable
parseReportTable contents = case Csv.decodeByName contents of
  Left reason -> Left $ ReportErrorUnparseable reason
  Right (header, rows) ->
    Right
      ReportTable
        { reportTableColumns = map (ReportColumn . TE.decodeUtf8Lenient) (V.toList header),
          reportTableRows = map ReportRow (V.toList rows)
        }

-- | The text in a column, which must be there.
requireColumn :: ReportTable -> ReportColumn -> ReportRow -> Either ReportError Text
requireColumn table column (ReportRow record) =
  case HM.lookup (TE.encodeUtf8 (unReportColumn column)) record of
    Nothing -> Left $ ReportErrorNoSuchColumn column (reportTableColumns table)
    Just raw -> case TE.decodeUtf8' raw of
      Left _ -> Left $ ReportErrorNotUtf8 column
      Right text -> Right (Text.strip text)

-- | The text in a column, treating an absent column as empty.
--
-- Only for columns Stripe leaves blank rather than omits: the payout rows of the
-- balance summary carry no currency at all.
columnOrEmpty :: ReportColumn -> ReportRow -> Text
columnOrEmpty column (ReportRow record) =
  maybe "" (Text.strip . TE.decodeUtf8Lenient) $
    HM.lookup (TE.encodeUtf8 (unReportColumn column)) record

-- | A money column, as an amount of the ledger's currency.
--
-- Major units in, minor units out, exactly or not at all.
requireAmount ::
  QuantisationFactor ->
  ReportTable ->
  ReportColumn ->
  ReportRow ->
  Either ReportError Money.Account
requireAmount quantisationFactor table column row = do
  raw <- requireColumn table column row
  literal <- requireLiteralOf column raw
  -- The library's own entry point for exactly this, rather than any arithmetic here:
  -- deciding whether a decimal fits a currency is its job, not this module's.
  case Account.fromDecimalLiteral quantisationFactor literal of
    Nothing -> Left $ ReportErrorNotRepresentable column raw quantisationFactor
    Just account -> Right account

-- | A plain decimal column that is not money, such as a tax rate.
requireRational :: ReportTable -> ReportColumn -> ReportRow -> Either ReportError Rational
requireRational table column row = do
  raw <- requireColumn table column row
  DecimalLiteral.toRational <$> requireLiteralOf column raw

requireLiteralOf :: ReportColumn -> Text -> Either ReportError DecimalLiteral
requireLiteralOf column raw =
  case DecimalLiteral.fromString (Text.unpack raw) of
    Nothing -> Left $ ReportErrorNotADecimal column raw
    Just literal -> Right literal

newtype ReportTypeId = ReportTypeId {unReportTypeId :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity ReportTypeId

instance HasCodec ReportTypeId where
  codec = dimapCodec ReportTypeId unReportTypeId codec

-- | A kind of report, and how much of it Stripe has finished computing.
data ReportType = ReportType
  { reportTypeId :: !ReportTypeId,
    -- | The earliest moment this report has data for.
    reportTypeDataAvailableStart :: !StripeTimestamp,
    -- | The latest moment this report has data for.
    --
    -- This is the completeness signal the importer waits on.  Stripe computes report
    -- data twice a day and fees lag further behind than everything else, so a month
    -- is not finished being reported on merely because it is over.
    reportTypeDataAvailableEnd :: !StripeTimestamp
  }
  deriving stock (Show, Eq, Generic)

instance Validity ReportType

instance HasCodec ReportType where
  codec =
    object "ReportType" $
      ReportType
        <$> requiredField "id" "identifier" .= reportTypeId
        <*> requiredField "data_available_start" "earliest data" .= reportTypeDataAvailableStart
        <*> requiredField "data_available_end" "latest data" .= reportTypeDataAvailableEnd

-- | Whether this report has data for everything up to and including a moment.
reportTypeCoversThrough :: StripeTimestamp -> ReportType -> Bool
reportTypeCoversThrough through reportType =
  reportTypeDataAvailableEnd reportType >= through
