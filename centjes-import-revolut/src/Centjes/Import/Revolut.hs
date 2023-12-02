{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Revolut (runCentjesImportRevolut) where

import Centjes.DecimalLiteral as DecimalLiteral
import Centjes.Format
import Centjes.Import.Revolut.OptParse
import Centjes.Location
import Centjes.Module
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Csv as Csv
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Vector as V
import Path
import System.Exit

runCentjesImportRevolut :: IO ()
runCentjesImportRevolut = do
  Settings {..} <- getSettings
  contents <- LB.fromStrict <$> SB.readFile (fromAbsFile settingInput)
  case Csv.decodeByName contents of
    Left err -> die err
    Right (_, v) -> do
      let ts =
            V.toList $
              V.map
                ( DeclarationTransaction
                    . noLoc
                    . rowTransaction settingAssetsAccountName settingExpensesAccountName settingFeesAccountName
                )
                v
      let m = Module {moduleImports = [], moduleDeclarations = ts}
      SB.writeFile (fromAbsFile settingOutput) (TE.encodeUtf8 (formatModule m))

rowTransaction :: AccountName -> AccountName -> AccountName -> Row -> Transaction ()
rowTransaction assetsAccountName expensesAccountName feeAccountName Row {..} =
  let transactionTimestamp = noLoc $ Timestamp $ localDay rowStartedDate
      -- TODO parse description
      transactionDescription = Just $ noLoc $ Description rowDescription
      assetPosting =
        noLoc
          Posting
            { postingAccountName = noLoc assetsAccountName,
              postingAccount = noLoc rowAmount,
              postingCurrencySymbol = noLoc (CurrencySymbol rowCurrency)
            }
      mFeePosting =
        if decimalLiteralScientific rowFee == 0
          then Nothing
          else
            Just $
              noLoc
                Posting
                  { postingAccountName = noLoc feeAccountName,
                    postingAccount = noLoc rowFee,
                    postingCurrencySymbol = noLoc (CurrencySymbol rowCurrency)
                  }
      leftover = rowAmount {decimalLiteralScientific = negate (decimalLiteralScientific rowAmount) - decimalLiteralScientific rowFee}
      leftoverPosting =
        noLoc
          Posting
            { postingAccountName = noLoc expensesAccountName,
              postingAccount = noLoc leftover,
              postingCurrencySymbol = noLoc (CurrencySymbol rowCurrency)
            }
      transactionPostings =
        concat
          [ [assetPosting],
            maybeToList mFeePosting,
            [leftoverPosting]
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
