{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Revolut (runCentjesImportRevolut) where

import Centjes.DecimalLiteral as DecimalLiteral
import Centjes.Format
import Centjes.Location
import Centjes.Module
import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Vector (Vector)
import Debug.Trace
import System.Environment
import System.Exit

runCentjesImportRevolut :: IO ()
runCentjesImportRevolut = do
  args <- getArgs
  case args of
    [] -> die "Usage: centjes-import-revolut <file.csv>"
    (file : _) -> do
      print file
      let revolutModule = Module [] []
      -- SB.writeFile file $ TE.encodeUtf8 $ formatModule revolutModule
      contents <- LB.fromStrict <$> SB.readFile file
      case Csv.decodeByName contents of
        Left err -> die err
        Right (h, v) -> do
          print h
          forM_ (v :: Vector Row) $ \r -> do
            print r
            let t = rowTransaction r
            putStrLn $ T.unpack $ formatTransaction t

rowTransaction :: Row -> Transaction ()
rowTransaction Row {..} =
  let transactionTimestamp = noLoc $ Timestamp $ localDay rowStartedDate
      -- TODO parse description
      transactionDescription = Just $ noLoc $ Description rowDescription
      transactionPostings =
        [ noLoc
            Posting
              { postingAccountName = noLoc (AccountName "assets:revolut"),
                postingAccount = noLoc rowAmount,
                postingCurrencySymbol = noLoc (CurrencySymbol rowCurrency)
              },
          noLoc
            Posting
              { postingAccountName = noLoc (AccountName "expenses:unknown"),
                postingAccount = noLoc $ DecimalLiteral.negate rowAmount,
                postingCurrencySymbol = noLoc (CurrencySymbol rowCurrency)
              }
        ]
   in Transaction {..}

data Row = Row
  { rowType :: !Text,
    rowProduct :: !Text,
    rowStartedDate :: !LocalTime,
    rowCompletedDate :: !(Maybe LocalTime),
    rowDescription :: !Text,
    rowAmount :: !DecimalLiteral,
    rowFee :: !DecimalLiteral,
    rowCurrency :: !Text,
    rowState :: !Text,
    rowBalance :: !(Maybe DecimalLiteral)
  }
  deriving (Show)

instance FromNamedRecord Row where
  parseNamedRecord r =
    Row
      <$> r
        .: "Type"
      <*> r
        .: "Product"
      <*> (r .: "Started Date" >>= parseTimeM True defaultTimeLocale "%F %H:%M:%S")
      <*> (r .: "Completed Date" >>= traverse (parseTimeM True defaultTimeLocale "%F %H:%M:%S"))
      <*> r
        .: "Description"
      <*> (r .: "Amount" >>= parseDecimalLiteralM)
      <*> (r .: "Fee" >>= parseDecimalLiteralM)
      <*> r
        .: "Currency"
      <*> r
        .: "State"
      <*> (r .: "Balance" >>= traverse parseDecimalLiteralM)
