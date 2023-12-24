{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland (runCentjesSwitzerland) where

import Centjes.Command.Check
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Switzerland.OptParse
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import qualified Money.Amount as Amount
import qualified Money.Amount as Money (Amount)
import Path
import Path.IO
import System.Process.Typed

runCentjesSwitzerland :: IO ()
runCentjesSwitzerland = do
  Settings {..} <- getSettings
  runStderrLoggingT $ do
    (declarations, diag) <- loadModules settingLedgerFile
    validation <- liftIO $ runValidationT $ doCompleteCheck declarations
    (ledger, _, _) <- liftIO $ checkValidation diag validation

    input <- liftIO $ checkValidation diag $ produceInput settingSetup ledger
    -- TODO Compile the templates into the binary
    mainTyp <- resolveFile' "templates/main.typ"
    outFile <- resolveFile' "example.pdf"
    liftIO $ compileTypstWithData input mainTyp outFile
    pure ()

produceInput :: Setup -> Ledger ann -> Validation (InputError ann) (Input ann)
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
          amount <- case account of
            Negative amount -> pure $ AmountWithCurrency amount currency
            Positive _ -> undefined -- TODO error
          pure $ Just amount
        else pure Nothing
    case V.toList relevantAccounts of
      [] -> pure Nothing
      [incomeAmount] -> do
        let incomeDay = Timestamp.toDay timestamp
        incomeEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> pure path
          _ -> undefined -- TODO error
        pure $ Just Income {..}
      _ -> undefined -- TODO Error
  pure Input {..}

data InputError ann = InputError

instance ToReport (InputError SourceSpan) where
  toReport = \case
    InputError -> undefined

data Input ann = Input
  { inputName :: Text,
    inputIncome :: Vector (Income ann)
  }
  deriving (Show)

instance ToJSON (Input ann) where
  toJSON Input {..} =
    object
      [ "name" .= inputName,
        "income" .= inputIncome
      ]

data Income ann = Income
  { incomeDay :: Day,
    incomeAmount :: AmountWithCurrency ann,
    incomeEvidence :: Path Rel File
  }
  deriving (Show)

instance ToJSON (Income ann) where
  toJSON Income {..} =
    object
      [ "day" .= incomeDay,
        "amount" .= incomeAmount,
        -- TODO prepend the income evidence directory
        "evidence" .= incomeEvidence
      ]

data AmountWithCurrency ann = AmountWithCurrency
  { amountWithCurrencyAmount :: Money.Amount,
    amountWithCurrencyCurrency :: Currency ann
  }
  deriving (Show)

instance ToJSON (AmountWithCurrency ann) where
  toJSON AmountWithCurrency {..} =
    let Located _ qf = currencyQuantisationFactor amountWithCurrencyCurrency
     in object
          [ "formatted" .= Amount.format qf amountWithCurrencyAmount,
            "symbol" .= CurrencySymbol.toText (currencySymbol amountWithCurrencyCurrency)
          ]

compileTypstWithData :: Input ann -> Path Abs File -> Path Abs File -> IO ()
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
