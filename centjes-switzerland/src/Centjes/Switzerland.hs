{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland (runCentjesSwitzerland, produceInputFromDeclarations) where

import Autodocodec
import Centjes.Command.Check
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Module (Declaration)
import Centjes.Switzerland.OptParse
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import qualified Money.Amount as Amount
import Path
import Path.IO
import System.Process.Typed

runCentjesSwitzerland :: IO ()
runCentjesSwitzerland = do
  Settings {..} <- getSettings
  runStderrLoggingT $ do
    (declarations, diag) <- loadModules settingLedgerFile
    validation <- liftIO $ runValidationT $ produceInputFromDeclarations settingSetup declarations
    input <- liftIO $ checkValidation diag validation
    -- TODO Compile the templates into the binary
    mainTyp <- resolveFile' "templates/main.typ"
    outFile <- resolveFile' "example.pdf"
    liftIO $ compileTypstWithData input mainTyp outFile
    pure ()

data AnyError ann
  = AnyErrorCheck (CheckError ann)
  | AnyErrorInput (InputError ann)

instance ToReport (AnyError SourceSpan) where
  toReport = \case
    AnyErrorCheck ce -> toReport ce
    AnyErrorInput ie -> toReport ie

produceInputFromDeclarations :: Setup -> [Declaration SourceSpan] -> ValidationT (AnyError SourceSpan) IO Input
produceInputFromDeclarations settingSetup declarations = do
  (ledger, _, _) <- mapValidationTFailure AnyErrorCheck $ doCompleteCheck declarations
  liftValidation $ mapValidationFailure AnyErrorInput $ produceInput settingSetup ledger

produceInput :: Setup -> Ledger ann -> Validation (InputError ann) Input
produceInput Setup {..} ledger = do
  let inputName = setupName
  inputIncome <- flip V.mapMaybeM (ledgerTransactions ledger) $ \(Located _ Transaction {..}) -> do
    let Located _ timestamp = transactionTimestamp
    relevantAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
      let Located _ accountName = postingAccountName
      if S.member accountName setupIncomeAccounts
        then do
          let Located _ account = postingAccount
          let Located _ currency = postingCurrency
          let Located _ qf = currencyQuantisationFactor currency
          let symbol = currencySymbol currency
          amount <- case account of
            Negative amount -> pure $ AmountWithCurrency (Amount.format qf amount) symbol
            Positive _ -> undefined -- TODO error
          pure $ Just amount
        else pure Nothing
    case V.toList relevantAccounts of
      [] -> pure Nothing
      [incomeAmount] -> do
        incomeEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> pure path
          _ -> undefined -- TODO error
        let incomeDay = Timestamp.toDay timestamp
        incomeDescription <- case transactionDescription of
          Nothing -> undefined -- TODO error
          Just (Located _ d) -> pure d
        pure $ Just Income {..}
      _ -> undefined -- TODO Error
  pure Input {..}

data InputError ann = InputError

instance ToReport (InputError SourceSpan) where
  toReport = \case
    InputError -> undefined

data Input = Input
  { inputName :: Text,
    inputIncome :: Vector Income
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "name" "name"
          .= inputName
        <*> requiredField "income" "income"
          .= inputIncome

data Income = Income
  { incomeDay :: !Day,
    incomeDescription :: !Description,
    incomeAmount :: !AmountWithCurrency,
    incomeEvidence :: !(Path Rel File)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Income)

instance HasCodec Income where
  codec =
    object "Income" $
      Income
        <$> requiredField "day" "day"
          .= incomeDay
        <*> requiredField "description" "description"
          .= incomeDescription
        <*> requiredField "amount" "amount"
          .= incomeAmount
        <*> requiredField "evidence" "evidence"
          .= incomeEvidence

data AmountWithCurrency = AmountWithCurrency
  { amountWithCurrencyAmount :: String,
    amountWithCurrencyCurrency :: CurrencySymbol
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec AmountWithCurrency)

instance HasCodec AmountWithCurrency where
  codec =
    object "AmountWithCurrency" $
      AmountWithCurrency
        <$> requiredField "formatted" "formatted amount"
          .= amountWithCurrencyAmount
        <*> requiredField "symbol" "currency symbol"
          .= amountWithCurrencyCurrency

compileTypstWithData :: Input -> Path Abs File -> Path Abs File -> IO ()
compileTypstWithData input rootFile outputFile = do
  print rootFile
  print outputFile
  jsonInputFile <- resolveFile (parent rootFile) "input.json"
  SB.writeFile (fromAbsFile jsonInputFile) (LB.toStrict (JSON.encode input))
  runProcess_ $
    setWorkingDir (fromAbsDir (parent rootFile)) $
      proc
        "typst"
        [ "compile",
          fromAbsFile rootFile,
          fromAbsFile outputFile,
          "--root",
          fromAbsDir (parent rootFile)
        ]
