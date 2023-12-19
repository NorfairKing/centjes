{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile
  ( CompileError (..),
    compileDeclarations,
    compileCurrencyDeclarationDeclarations,
    compileCurrencyDeclaration,
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
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import Money.QuantisationFactor
import qualified Money.QuantisationFactor as QuantisationFactor
import Numeric.DecimalLiteral as DecimalLiteral

data CompileError ann
  = CompileErrorInvalidQuantisationFactor !ann !CurrencySymbol !(GenLocated ann DecimalLiteral)
  | CompileErrorCurrencyDeclaredTwice !ann !ann !CurrencySymbol
  | CompileErrorMissingCurrency !ann !(GenLocated ann CurrencySymbol)
  | CompileErrorInvalidPrice !ann !(GenLocated ann DecimalLiteral)
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
              DecimalLiteral.format dl,
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
    CompileErrorMissingCurrency dl (Located sl symbol) ->
      Err
        (Just "CE_UNDECLARED_CURRENCY")
        ( unwords
            [ "Undeclared currency:",
              show (currencySymbolText symbol)
            ]
        )
        [ (toDiagnosePosition dl, Where "While trying to compile this declaration"),
          (toDiagnosePosition sl, This "this currency is never declared"),
          ( toDiagnosePosition dl,
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
    CompileErrorInvalidPrice pdl (Located ll dl) ->
      Err
        (Just "CE_INVALID_PRICE")
        ( unwords
            [ "Invalid price:",
              DecimalLiteral.format dl
            ]
        )
        [ (toDiagnosePosition ll, This "This rate is invalid"),
          (toDiagnosePosition pdl, Where "While compiling this price declaration")
        ]
        []
    CompileErrorUnparseableAmount tl (Located cl qf) (Located al dl) ->
      Err
        (Just "CE_INVALID_AMOUNT")
        ( unwords
            [ "Could not parse decimal literal as amount:",
              DecimalLiteral.format dl,
              "with quantisation factor",
              show (unQuantisationFactor qf)
            ]
        )
        [ (toDiagnosePosition tl, Where "while trying to compile this transaction"),
          (toDiagnosePosition al, This "This amount is invalid."),
          (toDiagnosePosition cl, Where "based on this currency declaration")
        ]
        []

unlines' :: [String] -> String
unlines' = intercalate "\n"

compileDeclarations :: [Declaration ann] -> Validation (CompileError ann) (Ledger ann)
compileDeclarations declarations = do
  ledgerCurrencies <- compileCurrencyDeclarationDeclarations declarations
  declarationPrices <- compilePriceDeclarations ledgerCurrencies declarations
  let transactions =
        mapMaybe
          ( \case
              DeclarationTransaction t -> Just t
              _ -> Nothing
          )
          declarations
  transactionTups <-
    traverse
      (compileTransaction ledgerCurrencies)
      transactions
  let ledgerTransactions =
        V.fromList . sortOnTimestamp Ledger.transactionTimestamp $
          map fst transactionTups
  let transactionPrices = concatMap snd transactionTups
  let ledgerPrices =
        V.fromList $
          sortOnTimestamp Ledger.priceTimestamp $
            declarationPrices ++ transactionPrices
  pure Ledger {..}

sortOnTimestamp :: (a -> GenLocated ann Timestamp) -> [GenLocated ann a] -> [GenLocated ann a]
sortOnTimestamp getTimestamp =
  sortBy
    ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
        `on` locatedValue
          . getTimestamp
          . locatedValue
    )

compileCurrencyDeclarationDeclarations ::
  [Declaration ann] ->
  Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
compileCurrencyDeclarationDeclarations declarations = do
  tups <-
    mapM
      compileCurrencyDeclaration
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

compileCurrencyDeclaration :: GenLocated ann (CurrencyDeclaration ann) -> Validation (CompileError ann) (CurrencySymbol, GenLocated ann QuantisationFactor)
compileCurrencyDeclaration (Located l CurrencyDeclaration {..}) = do
  let Located _ symbol = currencyDeclarationSymbol
  let Located _ dl = currencyDeclarationQuantisationFactor
  qf <- case QuantisationFactor.fromDecimalLiteral dl of
    Nothing -> validationFailure $ CompileErrorInvalidQuantisationFactor l symbol currencyDeclarationQuantisationFactor
    Just qf -> pure qf
  pure (symbol, Located l qf)

compilePriceDeclarations ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  [Declaration ann] ->
  Validation (CompileError ann) [GenLocated ann (Price ann)]
compilePriceDeclarations currencies =
  traverse (compilePriceDeclaration currencies)
    . mapMaybe
      ( \case
          DeclarationPrice pd -> Just pd
          _ -> Nothing
      )

compilePriceDeclaration ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (PriceDeclaration ann) ->
  Validation (CompileError ann) (GenLocated ann (Price ann))
compilePriceDeclaration currencies (Located pdl PriceDeclaration {..}) = do
  let priceTimestamp = priceDeclarationTimestamp
  priceCurrency <- compileCurrencyDeclarationSymbol currencies pdl priceDeclarationCurrencySymbol
  priceCost <- compileCostExpression currencies pdl priceDeclarationCost
  pure $ Located pdl Price {..}

compileCostExpression ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (CostExpression ann) ->
  Validation (CompileError ann) (GenLocated ann (Cost ann))
compileCostExpression currencies pdl (Located cl CostExpression {..}) = do
  costConversionRate <- compileConversionRate pdl costExpressionConversionRate
  costCurrency <- compileCurrencyDeclarationSymbol currencies pdl costExpressionCurrencySymbol
  pure $ Located cl Cost {..}

compileConversionRate ::
  ann ->
  GenLocated ann DecimalLiteral ->
  Validation (CompileError ann) (GenLocated ann Money.ConversionRate)
compileConversionRate pdl ldl@(Located rl dl) = do
  case ConversionRate.fromDecimalLiteral dl of
    Nothing -> validationFailure $ CompileErrorInvalidPrice pdl ldl
    Just cr -> pure (Located rl cr)

compileTransaction ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (Module.Transaction ann) ->
  Validation
    (CompileError ann)
    ( GenLocated ann (Ledger.Transaction ann),
      [GenLocated ann (Ledger.Price ann)]
    )
compileTransaction currencies (Located l mt) = do
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
  postings <- traverse (compilePosting currencies l) (Module.transactionPostings mt)
  let transactionPostings = V.fromList postings
  let prices =
        mapMaybe
          ( \(Located pl p) -> do
              lc <- Ledger.postingCost p
              pure $ Located pl $ Ledger.Price transactionTimestamp (postingCurrency p) lc
          )
          postings
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
  pure
    ( Located l Ledger.Transaction {..},
      prices
    )

compilePosting ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (Module.Posting ann) ->
  Validation
    (CompileError ann)
    (GenLocated ann (Ledger.Posting ann))
compilePosting currencies tl (Located l mp) = do
  let postingAccountName = Module.postingAccountName mp
  postingCurrency <- compileCurrencyDeclarationSymbol currencies tl (Module.postingCurrencySymbol mp)
  let lqf = currencyQuantisationFactor (locatedValue postingCurrency)
  postingAccount <- compileDecimalLiteral tl lqf (Module.postingAccount mp)
  postingCost <- mapM (compileCostExpression currencies tl) (Module.postingCost mp)
  pure (Located l Ledger.Posting {..})

compileAssertion ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (Module.Assertion ann) ->
  Validation (CompileError ann) (GenLocated ann (Ledger.Assertion ann))
compileAssertion currencies tl (Located l (Module.AssertionEquals lan ldl lqs)) = do
  lc <- compileCurrencyDeclarationSymbol currencies tl lqs
  let lqf = currencyQuantisationFactor (locatedValue lc)
  la <- compileDecimalLiteral tl lqf ldl
  pure (Located l (Ledger.AssertionEquals lan la lc))

compileCurrencyDeclarationSymbol ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann CurrencySymbol ->
  Validation (CompileError ann) (GenLocated ann (Currency ann))
compileCurrencyDeclarationSymbol currencies tl lcs@(Located cl symbol) = case M.lookup symbol currencies of
  Nothing -> validationFailure $ CompileErrorMissingCurrency tl lcs
  Just factor -> pure $ Located cl $ Currency {currencySymbol = symbol, currencyQuantisationFactor = factor}

compileDecimalLiteral ::
  ann ->
  GenLocated ann QuantisationFactor ->
  GenLocated ann DecimalLiteral ->
  Validation (CompileError ann) (GenLocated ann Money.Account)
compileDecimalLiteral tl lqf@(Located _ qf) ldl@(Located ll dl) = case Account.fromDecimalLiteral qf dl of
  Nothing -> validationFailure $ CompileErrorUnparseableAmount tl lqf ldl
  Just a -> pure (Located ll a)
