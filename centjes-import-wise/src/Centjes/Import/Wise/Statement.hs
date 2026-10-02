{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rows of a Wise balance statement, as exported or downloaded in CSV.
module Centjes.Import.Wise.Statement
  ( Row (..),
    decodeStatement,
    rowIsConversionLeg,
  )
where

import Centjes.CurrencySymbol as CurrencySymbol
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import Numeric.DecimalLiteral (DecimalLiteral)
import qualified Numeric.DecimalLiteral as DecimalLiteral

-- | One line of a balance statement.
--
-- A statement covers one balance, so one currency, but the currency is on every
-- row rather than on the file, which is what lets rows from several statements
-- be put in one list and sorted together.
--
-- Only the columns this importer acts on are here.  The counterparty names, the
-- card digits and the cardholder name are deliberately left unread: they are
-- personal data that would end up in a ledger in version control, and nothing
-- here needs them.
--
-- The exchange rate column is deliberately left unread too.  It is rounded to a
-- fixed number of decimals, so booking it would not reproduce the amounts it
-- sits between.  The rate this importer writes is the ratio of the two amounts
-- themselves, which is exact by construction.
data Row = Row
  { rowId :: !Text,
    rowDate :: !Day,
    -- | How much the balance moved, fee included.
    rowAmount :: !DecimalLiteral,
    rowCurrency :: !CurrencySymbol,
    rowDescription :: !Text,
    rowPaymentReference :: !Text,
    rowMerchant :: !Text,
    -- | What the balance stood at after this row.
    rowRunningBalance :: !(Maybe DecimalLiteral),
    rowExchangeFrom :: !(Maybe CurrencySymbol),
    rowExchangeTo :: !(Maybe CurrencySymbol),
    -- | The part of 'rowAmount' that Wise kept as a fee.
    rowTotalFees :: !(Maybe DecimalLiteral)
  }
  deriving (Show, Eq)

-- | Whether this row is one side of a conversion between two balances.
--
-- Wise writes a conversion into the statement of both balances, so a row that
-- says so has a counterpart in another statement.
rowIsConversionLeg :: Row -> Bool
rowIsConversionLeg row = case (rowExchangeFrom row, rowExchangeTo row) of
  (Just from, Just to) -> from /= to
  _ -> False

decodeStatement :: ByteString -> Either String [Row]
decodeStatement contents =
  V.toList . snd <$> Csv.decodeByName (LB.fromStrict (withoutByteOrderMark contents))

-- | A downloaded statement starts with a byte order mark.
--
-- It is part of the first header name as far as the CSV parser is concerned, so
-- leaving it on makes every column of a perfectly good statement look missing.
withoutByteOrderMark :: ByteString -> ByteString
withoutByteOrderMark contents = fromMaybe contents (SB.stripPrefix "\xef\xbb\xbf" contents)

instance FromNamedRecord Row where
  parseNamedRecord r =
    Row
      <$> r .: "TransferWise ID"
      <*> (r .: "Date" >>= parseStatementDay)
      <*> (r .: "Amount" >>= DecimalLiteral.fromStringM)
      <*> (r .: "Currency" >>= CurrencySymbol.fromTextM)
      <*> r .: "Description"
      <*> optionalText r "Payment Reference"
      <*> optionalText r "Merchant"
      <*> (r .: "Running Balance" >>= traverse DecimalLiteral.fromStringM)
      <*> (optionalColumn r "Exchange From" >>= traverse CurrencySymbol.fromTextM)
      <*> (optionalColumn r "Exchange To" >>= traverse CurrencySymbol.fromTextM)
      <*> (optionalColumn r "Total fees" >>= traverse DecimalLiteral.fromStringM)

-- | A column that need not be in the header at all.
--
-- Wise has changed which columns a statement carries, and a statement that is
-- missing one of these says the same thing as a statement whose cell is empty,
-- so neither is worth failing the whole import over.  The columns that carry
-- what a transaction *is* are not read this way: a statement without those is
-- not a statement this importer understands, and saying so is better than
-- quietly booking it wrong.
optionalColumn :: (FromField a) => NamedRecord -> ByteString -> Csv.Parser (Maybe a)
optionalColumn r columnName = case HM.lookup columnName r of
  Nothing -> pure Nothing
  Just _ -> r .: columnName

optionalText :: NamedRecord -> ByteString -> Csv.Parser Text
optionalText r columnName = fromMaybe T.empty <$> optionalColumn r columnName

-- | Wise dates in a statement are written day first.
--
-- An ISO day is accepted as well, because the JSON and CSV exports have not
-- always agreed on this and a date read the wrong way around is the kind of
-- mistake that only shows up months later.  The two formats cannot be confused
-- for one another: they put their separators in different places.
parseStatementDay :: (MonadFail m) => String -> m Day
parseStatementDay s =
  case parseTimeM True defaultTimeLocale "%d-%m-%Y" s of
    Just d -> pure d
    Nothing -> case parseTimeM True defaultTimeLocale "%F" s of
      Just d -> pure d
      Nothing -> fail $ unwords ["Not a date this importer can read:", show s]
