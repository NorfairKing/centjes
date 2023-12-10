{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Centjes.Format
import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Module as Module
import Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.DeepSeq
import Control.Monad
import Data.Function
import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Validity (Validity)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor
import Numeric.DecimalLiteral as DecimalLiteral

data CompileError ann
  = CompileErrorInvalidQuantisationFactor !ann !CurrencySymbol !(GenLocated ann DecimalLiteral)
  | CompileErrorCurrencyDeclaredTwice !ann !ann !CurrencySymbol
  | CompileErrorMissingCurrency !ann !(GenLocated ann CurrencySymbol)
  | CompileErrorUnparseableAmount !ann !(GenLocated ann QuantisationFactor) !(GenLocated ann DecimalLiteral)
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
              show (currencySymbolText sym)
            ]
        )
        [ (toDiagnosePosition fl, This "this number does not represent a valid quantisation factor"),
          (toDiagnosePosition cdl, Where "While trying to compile this currency declaration")
        ]
        []
    CompileErrorCurrencyDeclaredTwice cl1 cl2 symbol ->
      Err
        (Just "CE_DUPLICATE_CURRENCY")
        (unwords ["Currency has been declared twice:", show (currencySymbolText symbol)])
        [ (toDiagnosePosition cl1, Where "This currency has been declared here first"),
          (toDiagnosePosition cl2, This "This currency has been declared twice")
        ]
        []
    CompileErrorMissingCurrency tl (Located sl symbol) ->
      Err
        (Just "CE_UNDECLARED_CURRENCY")
        ( unwords
            [ "Undeclared currency:",
              show (currencySymbolText symbol)
            ]
        )
        [ (toDiagnosePosition tl, Where "While trying to compile this transaction"),
          (toDiagnosePosition sl, This "this currency is never declared"),
          ( toDiagnosePosition tl,
            Maybe $
              unlines'
                [ "You can declare this currency with a currency declaration:",
                  T.unpack $
                    T.strip $
                      formatDeclaration $
                        DeclarationCurrency $
                          noLoc $
                            CurrencyDeclaration
                              { currencyDeclarationSymbol = noLoc symbol,
                                currencyDeclarationQuantisationFactor = noLoc "0.01"
                              }
                ]
          )
        ]
        []
    CompileErrorUnparseableAmount tl (Located cl qf) (Located al dl) ->
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
          (toDiagnosePosition cl, Where "Based on this currency declaration")
        ]
        []

unlines' :: [String] -> String
unlines' = intercalate "\n"

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
  ledgerTransactions <-
    V.fromList
      . sortBy
        ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
            `on` locatedValue
              . Ledger.transactionTimestamp
              . locatedValue
        )
      <$> traverse
        (compileTransaction ledgerCurrencies)
        transactions
  pure Ledger {..}

-- TODO: rename to compileCurrencyDeclarations
compileCurrencies :: [Declaration ann] -> Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
compileCurrencies declarations = do
  tups <-
    mapM
      compileCurrency
      ( mapMaybe
          ( \case
              DeclarationCurrency cd -> Just cd
              _ -> Nothing
          )
          declarations
      )
  foldM go M.empty tups
  where
    go ::
      Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
      (CurrencySymbol, GenLocated ann QuantisationFactor) ->
      Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
    go m (symbol, lqf@(Located l2 _)) = case M.lookup symbol m of
      Nothing -> pure $ M.insert symbol lqf m
      Just (Located l1 _) -> validationFailure $ CompileErrorCurrencyDeclaredTwice l1 l2 symbol

-- TODO rename to compileCurrencyDeclaration
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
  transactionPostings <- V.fromList <$> traverse (compilePosting currencies l) (Module.transactionPostings mt)
  transactionAssertions <-
    V.fromList
      <$> traverse
        (compileAssertion currencies l)
        ( mapMaybe
            ( ( \case
                  TransactionAttachment _ -> Nothing
                  TransactionAssertion a -> Just a
              )
                . locatedValue
            )
            (Module.transactionExtras mt)
        )
  pure $ Located l Ledger.Transaction {..}

compilePosting ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (Module.Posting ann) ->
  Validation (CompileError ann) (GenLocated ann (Ledger.Posting ann))
compilePosting currencies tl (Located l mp) = do
  let postingAccountName = Module.postingAccountName mp
  postingCurrency <- compileCurrencySymbol currencies tl (Module.postingCurrencySymbol mp)
  let lqf = currencyQuantisationFactor (locatedValue postingCurrency)
  postingAccount <- compileDecimalLiteral tl lqf (Module.postingAccount mp)
  pure (Located l Ledger.Posting {..})

compileAssertion ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (Module.Assertion ann) ->
  Validation (CompileError ann) (GenLocated ann (Ledger.Assertion ann))
compileAssertion currencies tl (Located l (Module.AssertionEquals lan ldl lqs)) = do
  lc <- compileCurrencySymbol currencies tl lqs
  let lqf = currencyQuantisationFactor (locatedValue lc)
  la <- compileDecimalLiteral tl lqf ldl
  pure (Located l (Ledger.AssertionEquals lan la lc))

compileCurrencySymbol ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann CurrencySymbol ->
  Validation (CompileError ann) (GenLocated ann (Currency ann))
compileCurrencySymbol currencies tl lcs@(Located cl symbol) = case M.lookup symbol currencies of
  Nothing -> validationFailure $ CompileErrorMissingCurrency tl lcs
  Just factor -> pure $ Located cl $ Currency {currencySymbol = symbol, currencyQuantisationFactor = factor}

compileDecimalLiteral ::
  ann ->
  GenLocated ann QuantisationFactor ->
  GenLocated ann DecimalLiteral ->
  Validation (CompileError ann) (GenLocated ann Money.Account)
compileDecimalLiteral tl lqf@(Located _ qf) ldl@(Located ll dl) = case DecimalLiteral.toAccount qf dl of
  Nothing -> validationFailure $ CompileErrorUnparseableAmount tl lqf ldl
  Just a -> pure (Located ll a)
