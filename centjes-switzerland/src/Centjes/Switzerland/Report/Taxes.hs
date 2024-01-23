{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Report.Taxes
  ( produceTaxesInputFromDeclarations,
  )
where

import Autodocodec
import Centjes.Command.Check
import Centjes.Ledger
import Centjes.Location
import Centjes.Module (Declaration)
import Centjes.Report.Balance
import Centjes.Switzerland.OptParse
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Conduit
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import qualified Money.MultiAccount as MultiAccount
import Path

data TaxesError ann
  = TaxesErrorCheck (CheckError ann)
  | TaxesErrorInput (InputError ann)

instance ToReport (TaxesError SourceSpan) where
  toReport = \case
    TaxesErrorCheck ce -> toReport ce
    TaxesErrorInput ie -> toReport ie

produceTaxesInputFromDeclarations ::
  Setup ->
  [Declaration SourceSpan] ->
  ValidationT (TaxesError SourceSpan) IO (Input, Map (Path Rel File) (Path Rel File))
produceTaxesInputFromDeclarations setup declarations = do
  (ledger, balanceReport, _) <- mapValidationTFailure TaxesErrorCheck $ doCompleteCheck declarations
  transformValidationT
    ( \f -> do
        (v, fs) <- runWriterT f
        pure $ (,) <$> v <*> pure fs
    )
    $ mapValidationTFailure TaxesErrorInput
    $ produceInput setup ledger balanceReport

type P ann a = ValidationT (InputError ann) (WriterT (Map (Path Rel File) (Path Rel File)) IO) a

addEvidence :: Path Rel File -> Path Rel File -> P ann ()
addEvidence locationOnDisk locationInTarball = lift $ tell $ M.singleton locationInTarball locationOnDisk

produceInput ::
  Show ann =>
  Setup ->
  Ledger ann ->
  BalanceReport ann ->
  P ann Input
produceInput Setup {..} ledger BalanceReport {..} = do
  let inputName = setupName

  inputIncome <- flip V.mapMaybeM (ledgerTransactions ledger) $ \(Located _ Transaction {..}) -> do
    let Located _ timestamp = transactionTimestamp
    relevantAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
      let Located _ accountName = postingAccountName
      if M.member accountName setupIncomeAccounts
        then do
          let Located _ account = postingAccount
          let Located _ currency = postingCurrency
          amount <- case account of
            Negative amount -> pure $ amountToAmountWithCurrency currency amount
            Positive _ -> undefined -- TODO error
          pure $ Just amount
        else pure Nothing
    case V.toList relevantAccounts of
      [] -> pure Nothing
      [incomeAmount] -> do
        incomeEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> do
            let fileInTarball = [reldir|income|] </> path
            addEvidence path fileInTarball
            pure fileInTarball
          _ -> undefined -- TODO error
        let incomeDay = Timestamp.toDay timestamp
        incomeDescription <- case transactionDescription of
          Nothing -> undefined -- TODO error
          Just (Located _ d) -> pure d
        pure $ Just Income {..}
      -- TODO Error
      _ -> undefined

  inputExpenses <- flip V.mapMaybeM (ledgerTransactions ledger) $ \(Located _ Transaction {..}) -> do
    let Located _ timestamp = transactionTimestamp
    relevantAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
      let Located _ accountName = postingAccountName
      if S.member accountName setupExpensesAccounts
        then do
          let Located _ account = postingAccount
          let Located _ currency = postingCurrency
          amount <- case account of
            Negative _ -> undefined -- TODO error
            Positive amount -> pure $ amountToAmountWithCurrency currency amount
          pure $ Just amount
        else pure Nothing
    case V.toList relevantAccounts of
      [] -> pure Nothing
      [expenseAmount] -> do
        expenseEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> do
            let fileInTarball = [reldir|expense|] </> path
            addEvidence path fileInTarball
            pure fileInTarball
          _ -> undefined -- TODO error
        let expenseDay = Timestamp.toDay timestamp
        expenseDescription <- case transactionDescription of
          Nothing -> undefined -- TODO error
          Just (Located _ d) -> pure d
        pure $ Just Expense {..}
      -- TODO Error
      _ -> undefined

  inputAssets <- fmap V.fromList $ for (M.toList setupAssetsAccounts) $ \(name, AssetSetup {..}) -> do
    let assetName = name
    -- TODO check that file exists
    evidenceFile <- liftIO $ parseRelFile assetSetupEvidence
    let assetEvidence = [reldir|assets|] </> evidenceFile
    addEvidence evidenceFile assetEvidence
    assetAmount <- case M.lookup assetSetupAccountName balanceReportBalances of
      Nothing -> undefined -- TODO error
      Just ma -> case M.toList (MultiAccount.unMultiAccount ma) of
        [] -> undefined -- TODO error
        [(currency, account)] -> case account of
          Positive amount -> pure $ amountToAmountWithCurrency currency amount
          _ -> undefined -- TODO error
        m -> error (show m) -- TODO error
    pure Asset {..}

  pure Input {..}

amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
amountToAmountWithCurrency currency amount =
  let Located _ qf = currencyQuantisationFactor currency
      symbol = currencySymbol currency
   in AmountWithCurrency (Amount.format qf amount) symbol

data InputError ann = InputError

instance ToReport (InputError SourceSpan) where
  toReport = \case
    InputError -> undefined

data Input = Input
  { inputName :: Text,
    inputIncome :: Vector Income,
    inputAssets :: Vector Asset,
    inputExpenses :: Vector Expense
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
        <*> requiredField "assets" "assets"
          .= inputAssets
        <*> requiredField "expenses" "expenses"
          .= inputExpenses

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

data Asset = Asset
  { assetName :: !Text,
    assetAmount :: !AmountWithCurrency,
    assetEvidence :: !(Path Rel File)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Asset)

instance HasCodec Asset where
  codec =
    object "Asset" $
      Asset
        <$> requiredField "name" "asset account name"
          .= assetName
        <*> requiredField "amount" "amount"
          .= assetAmount
        <*> requiredField "evidence" "evidence"
          .= assetEvidence

data Expense = Expense
  { expenseDay :: !Day,
    expenseDescription :: !Description,
    expenseAmount :: !AmountWithCurrency,
    expenseEvidence :: !(Path Rel File)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Expense)

instance HasCodec Expense where
  codec =
    object "Expense" $
      Expense
        <$> requiredField "day" "expense day"
          .= expenseDay
        <*> requiredField "description" "expense description"
          .= expenseDescription
        <*> requiredField "amount" "amount"
          .= expenseAmount
        <*> requiredField "evidence" "evidence"
          .= expenseEvidence

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
