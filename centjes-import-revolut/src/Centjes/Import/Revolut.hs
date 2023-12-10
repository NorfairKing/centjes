{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Revolut (runCentjesImportRevolut) where

import Centjes.Compile
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Description as Description
import Centjes.Format
import Centjes.Import.Revolut.OptParse
import Centjes.Load
import Centjes.Location
import Centjes.Module
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Csv as Csv
import Data.Either
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
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

runCentjesImportRevolut :: IO ()
runCentjesImportRevolut = do
  Settings {..} <- getSettings
  let inputFp = fromAbsFile settingInput
  contents <- SB.readFile inputFp
  (ds, diag) <- runStderrLoggingT $ loadModules settingLedgerFile
  currencies <- checkValidation diag $ compileCurrencies ds

  case Csv.decodeByName (LB.fromStrict contents) of
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
                    (rowTransaction currencies settingAssetsAccountName settingExpensesAccountName settingFeesAccountName row)
            )
            (zip [1 ..] (sortOn (\r -> fromMaybe (rowStartedDate r) (rowCompletedDate r)) (V.toList v)))
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
rowTransaction currencies assetsAccountName expensesAccountName feeAccountName Row {..} = do
  -- TODO dont import non-completed transactions?
  -- rowAccount is the amount the expense
  -- rowFee is the fee on top of that amount
  let transactionTimestamp = noLoc $ Timestamp.secondFromLocalTime $ fromMaybe rowStartedDate rowCompletedDate
      transactionDescription =
        noLoc
          <$> Description.combine
            ( filter
                (not . T.null . unDescription)
                [ "Started date: " <> fromString (formatTime defaultTimeLocale "%F %T" rowStartedDate),
                  "Completed date: " <> fromString (maybe "" (formatTime defaultTimeLocale "%F %T") rowCompletedDate),
                  rowDescription
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
  expensesAccount <- Account.negate <$> fromLiteral rowAccount
  expensesLiteral <- toLiteral expensesAccount
  feeAccount <- fromLiteral rowFee
  feeLiteral <- toLiteral feeAccount
  assetsAccount <- case Account.add expensesAccount feeAccount of
    Nothing -> validationFailure $ ImportErrorAdd expensesAccount feeAccount
    Just l -> pure $ Account.negate l
  assetsLiteral <- toLiteral assetsAccount
  let mkPosting accountName literal =
        noLoc
          Posting
            { postingAccountName = noLoc accountName,
              postingAccount = noLoc literal,
              postingCurrencySymbol = noLoc rowCurrency
            }
  let assetPosting = mkPosting assetsAccountName assetsLiteral
  let mFeePosting =
        if feeAccount == Account.zero
          then Nothing
          else Just $ mkPosting feeAccountName feeLiteral
  let expensePosting = mkPosting expensesAccountName expensesLiteral
  let transactionPostings =
        concat
          [ [assetPosting],
            maybeToList mFeePosting,
            [expensePosting]
          ]
  mbl <- forM rowBalance $ \bal -> do
    balanceAccount <- fromLiteral bal
    toLiteral balanceAccount
  let transactionExtras =
        [ noLoc $
            TransactionAssertion $
              noLoc $
                AssertionEquals
                  (noLoc assetsAccountName)
                  (noLoc bl)
                  (noLoc rowCurrency)
          | bl <- maybeToList mbl
        ]
  pure Transaction {..}

data Row = Row
  { rowType :: !Text,
    rowProduct :: !Text,
    rowStartedDate :: !LocalTime,
    rowCompletedDate :: !(Maybe LocalTime),
    rowDescription :: !Description,
    rowAccount :: !DecimalLiteral,
    rowFee :: !DecimalLiteral,
    rowCurrency :: !CurrencySymbol,
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
      <*> (r .: "Description" >>= Description.fromTextM)
      <*> (r .: "Amount" >>= parseDecimalLiteralM)
      <*> (r .: "Fee" >>= parseDecimalLiteralM)
      <*> (r .: "Currency" >>= CurrencySymbol.fromTextM)
      <*> r
        .: "State"
      <*> (r .: "Balance" >>= traverse parseDecimalLiteralM)
