{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Neon (runCentjesImportNeon) where

import Centjes.Compile
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Description as Description
import Centjes.Format
import Centjes.Import.Neon.OptParse
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Validation
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Char as Char
import Data.Csv as Csv
import Data.Either
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.Vector as V
import Error.Diagnose
import qualified Money.Account as Account
import qualified Money.Account as Money (Account (..))
import Money.QuantisationFactor
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Path
import System.Exit

runCentjesImportNeon :: IO ()
runCentjesImportNeon = do
  Settings {..} <- getSettings
  let inputFp = fromAbsFile settingInput
  contents <- SB.readFile inputFp
  (ds, diag) <- runStderrLoggingT $ loadModules settingLedgerFile
  currencies <- checkValidation diag $ compileDeclarationsCurrencies ds

  let csvOpts = defaultDecodeOptions {decDelimiter = fromIntegral (Char.ord ';')}
  case Csv.decodeByNameWith csvOpts (LB.fromStrict contents) of
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
            (zip [1 ..] (sortOn rowDate (V.toList v)))
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
                (unwords ["Invalid amount:", show (DecimalLiteral.format dl), "with quantisation factor", show (unQuantisationFactor qf)])
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
  let transactionTimestamp = noLoc $ TimestampDay rowDate
      transactionDescription =
        noLoc
          <$> Description.combine
            ( filter
                (not . T.null . unDescription)
                [ rowDescription,
                  rowSubject
                ]
            )

  let rowCurrency = CurrencySymbol "CHF"
  Located _ quantisationFactor <- case M.lookup rowCurrency currencies of
    Nothing -> validationFailure $ ImportErrorUnknownCurrency rowCurrency
    Just qf -> pure qf
  let fromLiteral dl = case Account.fromDecimalLiteral quantisationFactor dl of
        Nothing -> validationFailure $ ImportErrorInvalidAccount quantisationFactor dl
        Just a -> pure a
  let toLiteral a = case Account.toDecimalLiteral quantisationFactor a of
        Nothing -> validationFailure $ ImportErrorInvalidLiteral quantisationFactor a
        Just dl -> pure dl
  expensesAccount <- Account.negate <$> fromLiteral rowAccount
  expensesLiteral <- toLiteral expensesAccount
  let assetsAccount = Account.negate expensesAccount
  assetsLiteral <- toLiteral assetsAccount
  let mkPosting accountName literal =
        noLoc
          Posting
            { postingReal = True,
              postingAccountName = noLoc accountName,
              postingAccount = noLoc literal,
              postingCurrencySymbol = noLoc rowCurrency,
              postingCost = Nothing,
              postingPercentage = Nothing
            }
  let assetPosting = mkPosting assetsAccountName assetsLiteral
  let expensePosting =
        mkPosting
          (if expensesAccount < Account.zero then incomeAccountName else expensesAccountName)
          expensesLiteral
  let transactionPostings =
        [ assetPosting,
          expensePosting
        ]
  let transactionExtras = []
  pure Transaction {..}

data Row = Row
  { rowDate :: !Day,
    rowAccount :: !DecimalLiteral,
    rowOriginalAccount :: !(Maybe DecimalLiteral),
    rowOriginalCurrency :: !(Maybe CurrencySymbol),
    rowExchangeRate :: !(Maybe DecimalLiteral),
    rowDescription :: !Description,
    rowSubject :: !Description,
    rowCategory :: !Description,
    rowTags :: !Description,
    rowWise :: !Bool,
    rowSpaces :: !Bool
  }

instance FromNamedRecord Row where
  parseNamedRecord r =
    Row
      <$> (r .: "Date" >>= parseTimeM True defaultTimeLocale "%F")
      <*> (r .: "Amount" >>= DecimalLiteral.fromStringM)
      <*> (r .: "Original amount" >>= traverse DecimalLiteral.fromStringM)
      <*> (r .: "Original currency" >>= traverse CurrencySymbol.fromTextM)
      <*> (r .: "Exchange rate" >>= traverse DecimalLiteral.fromStringM)
      <*> (r .: "Description" >>= Description.fromTextM)
      <*> (r .: "Subject" >>= Description.fromTextM)
      <*> (r .: "Category" >>= Description.fromTextM)
      <*> (r .: "Tags" >>= Description.fromTextM)
      <*> (r .: "Wise" >>= parseYesNoBool)
      <*> (r .: "Spaces" >>= parseYesNoBool)

parseYesNoBool :: MonadFail m => Text -> m Bool
parseYesNoBool = \case
  "yes" -> pure True
  "no" -> pure False
  b -> fail $ "Unknown bool: " <> show b
