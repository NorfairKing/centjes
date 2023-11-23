{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile
  ( CompileError (..),
    compileDeclarations,
    compileCurrencies,
    compileCurrency,
    compileTransaction,
    compilePosting,
  )
where

import Centjes.DecimalLiteral as DecimalLiteral
import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Module as Module
import Centjes.Validation
import Control.DeepSeq
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Traversable
import Data.Validity (Validity)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import Money.QuantisationFactor

data CompileError ann
  = CompileErrorInvalidQuantisationFactor !ann !CurrencySymbol !(GenLocated ann DecimalLiteral)
  | CompileErrorMissingCurrency !ann !ann !(GenLocated ann CurrencySymbol)
  | CompileErrorUnparseableAmount !ann !ann !(GenLocated ann QuantisationFactor) !(GenLocated ann DecimalLiteral)
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (CompileError ann)

instance NFData ann => NFData (CompileError ann)

instance ToReport (CompileError SourceSpan) where
  toReport = \case
    CompileErrorInvalidQuantisationFactor cdl sym (Located fl dl) ->
      Err
        (Just "CE_INVALID_QUANTISATION_FACTOR")
        ( unwords
            [ "Could not parse decimal literal as quantisation factor:",
              renderDecimalLiteral dl,
              "for currency with symbol",
              show (unCurrencySymbol sym)
            ]
        )
        [ (toDiagnosePosition fl, This "this number does not represent a valid quantisation factor"),
          (toDiagnosePosition cdl, Where "While trying to compile this currency declaration")
        ]
        []
    CompileErrorMissingCurrency tl pl (Located sl symbol) ->
      Err
        (Just "CE_UNDECLARED_CURRENCY")
        ( unwords
            [ "Undeclared currency:",
              show (unCurrencySymbol symbol)
            ]
        )
        [ (toDiagnosePosition tl, Where "While trying to compile this transaction"),
          (toDiagnosePosition sl, This "this currency is never declared"),
          (toDiagnosePosition pl, Where "While trying to compile this posting"),
          (toDiagnosePosition tl, Maybe "Declare this currency with a currency declaration") -- TODO
        ]
        []
    CompileErrorUnparseableAmount tl pl (Located cl qf) (Located al dl) ->
      Err
        (Just "CE_INVALID_AMOUNT")
        ( unwords
            [ "Could not parse decimal literal as amount:",
              renderDecimalLiteral dl,
              "with quantisation factor",
              show (unQuantisationFactor qf)
            ]
        )
        [ (toDiagnosePosition tl, Where "While trying to compile this transaction"),
          (toDiagnosePosition al, This "this amount is invalid"),
          (toDiagnosePosition pl, Where "While trying to compile this posting"),
          (toDiagnosePosition cl, Where "Based on this currency declaration")
        ]
        []

compileDeclarations :: [Declaration ann] -> Validation (CompileError ann) (Ledger ann)
compileDeclarations declarations = do
  ledgerCurrencies <- compileCurrencies declarations
  let transactions =
        mapMaybe
          ( \case
              DeclarationTransaction t -> Just t
              _ -> Nothing
          )
          declarations
  ledgerTransactions <- V.fromList . sortOn (locatedValue . Ledger.transactionTimestamp . locatedValue) <$> traverse (compileTransaction ledgerCurrencies) transactions
  pure Ledger {..}

compileCurrencies :: [Declaration ann] -> Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
compileCurrencies declarations =
  -- TODO check for duplicate currencies
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

compileCurrency :: GenLocated ann (CurrencyDeclaration ann) -> Validation (CompileError ann) (CurrencySymbol, GenLocated ann QuantisationFactor)
compileCurrency (Located l CurrencyDeclaration {..}) = do
  let Located _ symbol = currencyDeclarationSymbol
  let Located _ dl = currencyDeclarationQuantisationFactor
  qf <- case DecimalLiteral.toQuantisationFactor dl of
    Nothing -> validationFailure $ CompileErrorInvalidQuantisationFactor l symbol currencyDeclarationQuantisationFactor
    Just qf -> pure qf
  pure (symbol, Located l qf)

compileTransaction ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (Module.Transaction ann) ->
  Validation (CompileError ann) (GenLocated ann (Ledger.Transaction ann))
compileTransaction currencies (Located l mt) = do
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
  transactionPostings <- traverse (compilePosting currencies l) (Module.transactionPostings mt)
  pure $ Located l Ledger.Transaction {..}

compilePosting ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (Module.Posting ann) ->
  Validation (CompileError ann) (GenLocated ann (Ledger.Posting ann))
compilePosting currencies tl (Located l mp) = do
  let postingAccountName = Module.postingAccountName mp
  postingCurrency <- for (Module.postingCurrencySymbol mp) $ \symbol -> case M.lookup symbol currencies of
    Nothing -> validationFailure $ CompileErrorMissingCurrency tl l (Module.postingCurrencySymbol mp)
    Just factor -> pure $ Currency {currencySymbol = symbol, currencyQuantisationFactor = factor}
  let lqf@(Located _ qf) = currencyQuantisationFactor (locatedValue postingCurrency)
  postingAccount <- for (Module.postingAccount mp) $ \df -> case DecimalLiteral.toAccount qf df of
    Nothing -> validationFailure $ CompileErrorUnparseableAmount tl l lqf (Module.postingAccount mp)
    Just a -> pure a
  pure (Located l Ledger.Posting {..})
