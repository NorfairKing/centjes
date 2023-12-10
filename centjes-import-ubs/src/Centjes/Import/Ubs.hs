{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Ubs (runCentjesImportUbs) where

import Centjes.Compile
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Description as Description
import Centjes.Format
import Centjes.Import.Ubs.OptParse
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Validation
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char (ord)
import Data.Csv as Csv
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Vector as V
import Error.Diagnose
import qualified Money.Account as Account
import qualified Money.Account as Money (Account (..))
import Money.QuantisationFactor
import Numeric.DecimalLiteral as DecimalLiteral
import Path
import System.Exit

runCentjesImportUbs :: IO ()
runCentjesImportUbs = do
  Settings {..} <- getSettings
  let inputFp = fromAbsFile settingInput
  contents <- SB.readFile inputFp
  (ds, diag) <- runStderrLoggingT $ loadModules settingLedgerFile
  currencies <- checkValidation diag $ compileCurrencies ds

  let decodeOpts = Csv.defaultDecodeOptions {decDelimiter = fromIntegral (Char.ord ';')}
  case Csv.decodeByNameWith decodeOpts (LB.fromStrict $ SB8.unlines $ drop 9 $ SB8.lines contents) of
    Left e -> die e
    Right (_, v) -> do
      ts <-
        checkValidation
          ( addFile
              diag
              inputFp
              (T.unpack (fromRight "No file info because it was not UTF8" (TE.decodeUtf8' contents)))
          )
          $ traverse
            ( \(rowIx, row) ->
                DeclarationTransaction . noLoc
                  <$> mapValidationFailure
                    (ImportError inputFp rowIx)
                    ( rowTransaction
                        currencies
                        settingAssetsAccountName
                        settingExpensesAccountName
                        settingIncomeAccountName
                        row
                    )
            )
            (zip [10 ..] (reverse (V.toList v)))
      let m = Module {moduleImports = [], moduleDeclarations = ts}
      SB.writeFile (fromAbsFile settingOutput) (TE.encodeUtf8 (formatModule m))

data ImportError = ImportError !FilePath !Int ImportError'

data ImportError'
  = ImportErrorUnknownCurrency !CurrencySymbol
  | ImportErrorInvalidAccount !QuantisationFactor !DecimalLiteral
  | ImportErrorInvalidLiteral !QuantisationFactor !Money.Account
  | ImportErrorAdd !Money.Account !Money.Account

instance ToReport ImportError where
  toReport (ImportError fp rowIx ie) =
    let f =
          case ie of
            ImportErrorUnknownCurrency symbol ->
              Err
                (Just "IE_REVOLUT_UNKNOWN_CURRENCY")
                (unwords ["Unknown currency:", show (currencySymbolText symbol)])
            ImportErrorInvalidAccount qf dl ->
              Err
                (Just "IE_REVOLUT_INVALID_AMOUNT")
                (unwords ["Invalid amount:", show (renderDecimalLiteral dl), "with quantisation factor", show (unQuantisationFactor qf)])
            ImportErrorInvalidLiteral qf a ->
              Err
                (Just "IE_REVOLUT_INVALID_LITERAL")
                (unwords ["Invalid literal:", show a, "with quantisation factor", show (unQuantisationFactor qf)])
            ImportErrorAdd a1 a2 ->
              Err
                (Just "IE_REVOLUT_ADD")
                (unwords ["Could not add", show a2, "to", show a1])
     in f
          [ ( Position
                { begin = (rowIx, 0),
                  -- There's no way to represent "end of this line".
                  end = (rowIx, 1000),
                  file = fp
                },
              Where "While importing this row"
            )
          ]
          []

rowTransaction ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  AccountName ->
  AccountName ->
  AccountName ->
  Row ->
  Validation ImportError' (Transaction ())
rowTransaction currencies assetsAccountName expensesAccountName incomeAccountName Row {..} = do
  -- rowAccount is the amount the expense
  -- rowFee is the fee on top of that amount
  let transactionTimestamp = noLoc $ TimestampDay rowBookingDate
      transactionDescription =
        noLoc
          <$> Description.combine
            ( filter
                (not . T.null . unDescription)
                [ "Trade date: " <> case rowTradeTime of
                    Nothing -> fromString (formatTime defaultTimeLocale "%F" rowTradeDate)
                    Just tt -> fromString (formatTime defaultTimeLocale "%F %T" (LocalTime rowValueDate tt)),
                  "Booking date: " <> fromString (formatTime defaultTimeLocale "%F" rowBookingDate),
                  "Value date: " <> fromString (formatTime defaultTimeLocale "%F" rowValueDate),
                  rowDescription1,
                  rowDescription2,
                  rowDescription3,
                  rowFootnotes
                ]
            )

  Located _ quantisationFactor <- case M.lookup rowCurrency currencies of
    Nothing -> validationFailure $ ImportErrorUnknownCurrency rowCurrency
    Just qf -> pure qf

  let fromLiteral dl = case DecimalLiteral.toAccount quantisationFactor dl of
        Nothing -> validationFailure $ ImportErrorInvalidAccount quantisationFactor dl
        Just a -> pure a
  let toLiteral a = case DecimalLiteral.fromAccount quantisationFactor a of
        Nothing -> validationFailure $ ImportErrorInvalidLiteral quantisationFactor a
        Just dl -> pure dl

  assetsAccount <- case rowCredit of
    Just dl -> fromLiteral dl
    Nothing -> case rowDebit of
      Just dl -> fromLiteral dl
      Nothing -> undefined
  assetsLiteral <- toLiteral assetsAccount

  let otherAccount = Account.negate assetsAccount
  otherLiteral <- toLiteral otherAccount

  balanceAccount <- fromLiteral rowBalance
  balanceLiteral <- toLiteral balanceAccount

  let mkPosting accountName literal =
        noLoc
          Posting
            { postingAccountName = noLoc accountName,
              postingAccount = noLoc literal,
              postingCurrencySymbol = noLoc rowCurrency
            }

  let assetsPosting = mkPosting assetsAccountName assetsLiteral
  let otherPosting =
        mkPosting
          ( if otherAccount >= Account.zero
              then expensesAccountName
              else incomeAccountName
          )
          otherLiteral

  let transactionPostings =
        [ assetsPosting,
          otherPosting
        ]
  let transactionExtras =
        [ noLoc $
            TransactionAssertion $
              noLoc $
                AssertionEquals
                  (noLoc assetsAccountName)
                  (noLoc balanceLiteral)
                  (noLoc rowCurrency)
        ]
  pure Transaction {..}

data Row = Row
  { rowTradeDate :: !Day,
    rowTradeTime :: !(Maybe TimeOfDay),
    rowBookingDate :: !Day,
    rowValueDate :: !Day,
    rowCurrency :: !CurrencySymbol,
    rowDebit :: !(Maybe DecimalLiteral),
    rowCredit :: !(Maybe DecimalLiteral),
    rowIndividualAmount :: !(Maybe DecimalLiteral),
    rowBalance :: !DecimalLiteral,
    rowTransactionNo :: !Text,
    rowDescription1 :: !Description,
    rowDescription2 :: !Description,
    rowDescription3 :: !Description,
    rowFootnotes :: !Description
  }
  deriving (Show)

instance FromNamedRecord Row where
  parseNamedRecord r =
    Row
      <$> (r .: "Trade date" >>= parseTimeM True defaultTimeLocale "%F")
      <*> (r .: "Trade time" >>= traverse (parseTimeM True defaultTimeLocale "%T"))
      <*> (r .: "Booking date" >>= parseTimeM True defaultTimeLocale "%F")
      <*> (r .: "Value date" >>= parseTimeM True defaultTimeLocale "%F")
      <*> (r .: "Currency" >>= CurrencySymbol.fromTextM)
      <*> (r .: "Debit" >>= traverse parseDecimalLiteralM)
      <*> (r .: "Credit" >>= traverse parseDecimalLiteralM)
      <*> (r .: "Individual amount" >>= traverse parseDecimalLiteralM)
      <*> (r .: "Balance" >>= parseDecimalLiteralM)
      <*> r
        .: "Transaction no."
      <*> (r .: "Description1" >>= Description.fromTextM)
      <*> (r .: "Description2" >>= Description.fromTextM)
      <*> (r .: "Description3" >>= Description.fromTextM)
      <*> (r .: "Footnotes" >>= Description.fromTextM)
