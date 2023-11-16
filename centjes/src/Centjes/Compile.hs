{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile
  ( CompileError (..),
    compileDeclarations,
    compileTransaction,
    compilePosting,
  )
where

import Centjes.DecimalLiteral as DecimalLiteral
import Centjes.Ledger as Ledger
import Centjes.Module as Module
import Centjes.Validation
import Control.Exception
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Validity (Validity)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Money.QuantisationFactor

data CompileError
  = CompileErrorInvalidQuantisationFactor !CurrencySymbol !DecimalLiteral
  | CompileErrorUnparseableAmount !QuantisationFactor !DecimalLiteral
  | CompileErrorMissingCurrency !CurrencySymbol
  deriving (Show, Eq, Generic)

instance Validity CompileError

instance Exception CompileError where
  displayException = \case
    CompileErrorInvalidQuantisationFactor sym dl ->
      unwords
        [ "Could not parse decimal literal as quantisation factor:",
          renderDecimalLiteral dl,
          "for currency with symbol",
          show sym
        ]
    CompileErrorUnparseableAmount qf dl ->
      unwords
        [ "Could not parse decimal literal as amount:",
          renderDecimalLiteral dl,
          "with quantisation factor",
          show qf
        ]
    CompileErrorMissingCurrency symbol ->
      unwords
        [ "Undeclared currency:",
          show symbol
        ]

compileDeclarations :: [Declaration] -> Validation CompileError Ledger
compileDeclarations declarations = do
  -- TODO check for duplicate currencies
  ledgerCurrencies <-
    M.fromList
      <$> mapM
        compileCurrency
        ( mapMaybe
            ( \case
                DeclarationCurrency cd -> Just cd
                _ -> Nothing
            )
            declarations
        )
  let transactions =
        mapMaybe
          ( \case
              DeclarationTransaction t -> Just t
              _ -> Nothing
          )
          declarations
  ledgerTransactions <- V.fromList . sort <$> traverse (compileTransaction ledgerCurrencies) transactions
  pure Ledger {..}

compileCurrency :: CurrencyDeclaration -> Validation CompileError (CurrencySymbol, QuantisationFactor)
compileCurrency CurrencyDeclaration {..} = do
  let symbol = currencyDeclarationSymbol
  let dl = currencyDeclarationQuantisationFactor
  qf <- case DecimalLiteral.toQuantisationFactor dl of
    Nothing -> validationFailure $ CompileErrorInvalidQuantisationFactor currencyDeclarationSymbol dl
    Just qf -> pure qf
  pure (symbol, qf)

compileTransaction ::
  Map CurrencySymbol QuantisationFactor ->
  Module.Transaction ->
  Validation CompileError Ledger.Transaction
compileTransaction currencies mt = do
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
  transactionPostings <- traverse (compilePosting currencies) (Module.transactionPostings mt)
  pure Ledger.Transaction {..}

compilePosting ::
  Map CurrencySymbol QuantisationFactor ->
  Module.Posting ->
  Validation CompileError Ledger.Posting
compilePosting currencies mp = do
  let postingAccountName = Module.postingAccountName mp
      symbol = Module.postingCurrencySymbol mp
  postingCurrency <- case M.lookup symbol currencies of
    Nothing -> validationFailure $ CompileErrorMissingCurrency symbol
    Just factor -> pure $ Currency {currencySymbol = symbol, currencyFactor = factor}
  let df = Module.postingAccount mp
  let qf = currencyFactor postingCurrency
  postingAccount <- case DecimalLiteral.toAccount qf df of
    Nothing -> validationFailure $ CompileErrorUnparseableAmount qf df
    Just a -> pure a
  pure Ledger.Posting {..}
