{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Report.VAT
  ( produceVATInputFromDeclarations,
  )
where

import Autodocodec
import Centjes.Command.Check
import Centjes.Convert
import Centjes.Ledger
import Centjes.Location
import Centjes.Module (Declaration)
import Centjes.Switzerland.OptParse
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Conduit
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.Quarter
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Path

data VATError ann
  = VATErrorCheck (CheckError ann)
  | VATErrorInput (InputError ann)

instance ToReport (VATError SourceSpan) where
  toReport = \case
    VATErrorCheck ce -> toReport ce
    VATErrorInput ie -> toReport ie

produceVATInputFromDeclarations ::
  Setup ->
  [Declaration SourceSpan] ->
  ValidationT (VATError SourceSpan) IO (Input, Map (Path Rel File) (Path Rel File))
produceVATInputFromDeclarations setup declarations = do
  (ledger, _, _) <- mapValidationTFailure VATErrorCheck $ doCompleteCheck declarations
  transformValidationT
    ( \f -> do
        (v, fs) <- runWriterT f
        pure $ (,) <$> v <*> pure fs
    )
    $ mapValidationTFailure VATErrorInput
    $ produceInput setup ledger

type P ann a = ValidationT (InputError ann) (WriterT (Map (Path Rel File) (Path Rel File)) IO) a

addEvidence :: Path Rel File -> Path Rel File -> P ann ()
addEvidence locationOnDisk locationInTarball = lift $ tell $ M.singleton locationOnDisk locationInTarball

produceInput ::
  Ord ann =>
  Setup ->
  Ledger ann ->
  P ann Input
produceInput Setup {..} ledger = do
  let inputName = setupName
  let inputQuarter = fromMaybe (YearQuarter 2024 Q1) setupVATQuarter

  let chfSymbol = CurrencySymbol "CHF"
  chfCurrency <- case M.lookup chfSymbol (ledgerCurrencies ledger) of
    Nothing -> undefined
    Just lqf -> pure $ Currency {currencySymbol = chfSymbol, currencyQuantisationFactor = lqf}

  let priceGraph = pricesToPriceGraph (ledgerPrices ledger)

  inputIncome <- flip V.mapMaybeM (ledgerTransactions ledger) $ \(Located _ Transaction {..}) -> do
    let Located _ timestamp = transactionTimestamp
    let day = Timestamp.toDay timestamp
    if periodFirstDay inputQuarter <= day && day <= periodLastDay inputQuarter
      then do
        let incomeDay = Timestamp.toDay timestamp
        incomeDescription <- case transactionDescription of
          Nothing -> undefined -- TODO error
          Just (Located _ d) -> pure d
        incomeEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> do
            let fileInTarball = [reldir|income|] </> path
            addEvidence path fileInTarball
            pure fileInTarball
          _ -> undefined -- TODO error
        relevantAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
          let Located _ accountName = postingAccountName
          if M.member accountName setupIncomeAccounts
            then do
              let Located _ account = postingAccount
              let Located _ currency = postingCurrency
              case account of
                Negative amount -> pure $ Just (amount, currency)
                Positive _ -> undefined -- TODO error
            else pure Nothing
        relevantVATAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
          let Located _ accountName = postingAccountName
          if accountName == setupVATIncomeAccount
            then do
              let Located _ account = postingAccount
              let Located _ currency = postingCurrency
              -- TODO check that the currency is CHF
              case account of
                Negative amount -> pure $ Just $ amountToAmountWithCurrency currency amount
                Positive _ -> undefined -- TODO error
            else pure Nothing
        case V.toList relevantAccounts of
          [] -> pure Nothing
          [(amount, currency)] -> do
            let incomeAmount = amountToAmountWithCurrency currency amount
            incomeVAT <- case V.toList relevantVATAccounts of
              [] -> pure Nothing
              [vatAmount] -> pure $ Just vatAmount
              _ -> undefined
            -- TODO use the conversion rate from the appropriate date
            -- TODO figure out if this is the correct rounding
            incomeCHFAmount <- amountToAmountWithCurrency chfCurrency <$> mapValidationTFailure InputErrorConvert (liftValidation (convertAmount priceGraph RoundNearest currency amount chfCurrency))
            pure $ Just Income {..}
          -- TODO Error
          _ -> undefined
      else pure Nothing

  pure Input {..}

amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
amountToAmountWithCurrency currency amount =
  let Located _ qf = currencyQuantisationFactor currency
      symbol = currencySymbol currency
   in AmountWithCurrency (Amount.format qf amount) symbol

data InputError ann
  = InputErrorConvert (ConvertError ann)

instance ToReport (InputError SourceSpan) where
  toReport = \case
    InputErrorConvert ce -> toReport ce

-- TODO split up "Input" into "Input to Typst" and "VAT report"
data Input = Input
  { inputName :: Text,
    inputQuarter :: !Quarter,
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
        <*> requiredFieldWith "quarter" (codecViaAeson "Quarter") "quarter"
          .= inputQuarter
        <*> requiredField "income" "income"
          .= inputIncome

data Income = Income
  { incomeDay :: !Day,
    incomeDescription :: !Description,
    incomeAmount :: !AmountWithCurrency,
    incomeVAT :: !(Maybe AmountWithCurrency),
    incomeCHFAmount :: !AmountWithCurrency,
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
        <*> requiredField "vat" "vat"
          .= incomeVAT
        <*> requiredField "chf" "amount in chf"
          .= incomeCHFAmount
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
