{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile
  ( CompileError (..),
    compileDeclarations,
    compileCurrencyDeclarations,
    compileCurrencyDeclaration,
    compileTransaction,
    compilePosting,
  )
where

import qualified Centjes.AccountName as AccountName
import qualified Centjes.AccountType as AccountType
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
import Data.Ratio
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
import Numeric.Natural

data CompileError ann
  = CompileErrorInvalidQuantisationFactor !ann !CurrencySymbol !(GenLocated ann DecimalLiteral)
  | CompileErrorMissingCurrency !ann !(GenLocated ann CurrencySymbol)
  | CompileErrorCurrencyDeclaredTwice !ann !ann !CurrencySymbol
  | CompileErrorMissingAccount !ann !(GenLocated ann AccountName)
  | CompileErrorAccountDeclaredTwice !ann !ann !AccountName
  | CompileErrorCouldNotInferAccountType !ann !(GenLocated ann AccountName)
  | CompileErrorInvalidPrice !ann !(GenLocated ann (RationalExpression ann))
  | CompileErrorInvalidPercentage !ann !ann !(GenLocated ann (RationalExpression ann))
  | CompileErrorInvalidRational !ann !(GenLocated ann (RationalExpression ann))
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
    CompileErrorCurrencyDeclaredTwice cl1 cl2 symbol ->
      Err
        (Just "CE_DUPLICATE_CURRENCY")
        (unwords ["Currency has been declared twice:", show (currencySymbolText symbol)])
        [ (toDiagnosePosition cl1, Where "This currency has been declared here first"),
          (toDiagnosePosition cl2, This "This currency has been declared twice")
        ]
        []
    CompileErrorMissingAccount dl (Located anl an) ->
      Err
        (Just "CE_UNDECLARED_ACCOUNT")
        ( unwords
            [ "Undeclared account:",
              AccountName.toString an
            ]
        )
        [ (toDiagnosePosition dl, Where "While trying to compile this declaration"),
          (toDiagnosePosition anl, This "This account is never declared.")
        ]
        [ Hint $
            unlines'
              [ "You can declare this account with an account declaration:",
                T.unpack $
                  T.strip $
                    formatDeclaration $
                      DeclarationAccount $
                        noLoc $
                          AccountDeclaration
                            { accountDeclarationName = noLoc an,
                              accountDeclarationType = case AccountType.fromAccountName an of
                                Nothing -> Just $ noLoc AccountTypeAssets
                                Just _ -> Nothing
                            }
              ]
        ]
    CompileErrorAccountDeclaredTwice al1 al2 an ->
      Err
        (Just "CE_DUPLICATE_ACCOUNT")
        (unwords ["Account has been declared twice:", AccountName.toString an])
        [ (toDiagnosePosition al1, Where "This account has been declared here first"),
          (toDiagnosePosition al2, This "This account has been declared twice")
        ]
        []
    CompileErrorCouldNotInferAccountType dl (Located anl an) ->
      Err
        (Just "CE_INFER_ACCOUNT_TYPE")
        "Could not infer account type"
        [ (toDiagnosePosition anl, This "While trying to infer the account type of this account based on its name."),
          (toDiagnosePosition dl, Where "While trying to compile this declaration")
        ]
        [ Hint $
            unlines'
              [ "You can declare the account type explicitly as follows:",
                unwords
                  [ "account",
                    AccountName.toString an,
                    "assets"
                  ]
              ]
        ]
    CompileErrorInvalidPrice dl (Located ll _) ->
      Err
        (Just "CE_INVALID_PRICE")
        "Invalid price, cannot be interpreted as a positive fraction."
        [ (toDiagnosePosition ll, This "This rate is invalid"),
          (toDiagnosePosition dl, Where "While compiling this declaration")
        ]
        []
    CompileErrorInvalidPercentage tl pel (Located ll _) ->
      Err
        (Just "CE_INVALID_PERCENTAGE")
        "Invalid percentage, cannot be interpreted as a positive fraction."
        [ (toDiagnosePosition ll, This "This percentage is invalid"),
          (toDiagnosePosition pel, Where "While compiling this percentage"),
          (toDiagnosePosition tl, Where "While compiling this transaction")
        ]
        []
    CompileErrorInvalidRational tl (Located ll _) ->
      Err
        (Just "CE_INVALID_RATIONAL")
        "Invalid rational, cannot be interpreted as a positive fraction."
        [ (toDiagnosePosition ll, This "This rational expression is invalid"),
          (toDiagnosePosition tl, Blank)
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
  ledgerCurrencies <- compileCurrencyDeclarations declarations
  ledgerAccounts <- compileAccountDeclarations declarations
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
      (compileTransaction ledgerCurrencies ledgerAccounts)
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

compileCurrencyDeclarations ::
  [Declaration ann] ->
  Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
compileCurrencyDeclarations declarations = do
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

compileAccountDeclarations ::
  [Declaration ann] ->
  Validation (CompileError ann) (Map AccountName (GenLocated ann AccountType))
compileAccountDeclarations declarations = do
  tups <-
    mapM
      compileAccountDeclaration
      ( mapMaybe
          ( \case
              DeclarationAccount ad -> Just ad
              _ -> Nothing
          )
          declarations
      )
  foldM go M.empty tups
  where
    go ::
      Map AccountName (GenLocated ann AccountType) ->
      (AccountName, GenLocated ann AccountType) ->
      Validation (CompileError ann) (Map AccountName (GenLocated ann AccountType))
    go m (an, lat@(Located l2 _)) = case M.lookup an m of
      Nothing -> pure $ M.insert an lat m
      Just (Located l1 _) -> validationFailure $ CompileErrorAccountDeclaredTwice l1 l2 an

compileAccountDeclaration ::
  GenLocated ann (AccountDeclaration ann) ->
  Validation (CompileError ann) (AccountName, GenLocated ann AccountType)
compileAccountDeclaration (Located dl AccountDeclaration {..}) = do
  let Located _ name = accountDeclarationName
  typ <- case accountDeclarationType of
    Just (Located _ t) -> pure t
    Nothing -> case AccountType.fromAccountName name of
      Just t -> pure t
      Nothing -> validationFailure $ CompileErrorCouldNotInferAccountType dl accountDeclarationName
  pure (name, Located dl typ)

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
  GenLocated ann (RationalExpression ann) ->
  Validation (CompileError ann) (GenLocated ann Money.ConversionRate)
compileConversionRate pdl lre = do
  Located rl r <-
    mapValidationFailure
      ( \case
          CompileErrorInvalidRational _ _ -> CompileErrorInvalidPrice pdl lre
          e -> e
      )
      (compileRational pdl lre)
  case ConversionRate.fromRatio r of
    Nothing -> validationFailure $ CompileErrorInvalidPrice pdl lre
    Just cr -> pure (Located rl cr)

compilePercentageExpression ::
  ann ->
  GenLocated ann (PercentageExpression ann) ->
  Validation (CompileError ann) (GenLocated ann (Percentage ann))
compilePercentageExpression pl (Located pel PercentageExpression {..}) = do
  unPercentage <- compilePercentage pl pel unPercentageExpression
  pure $ Located pel Percentage {..}

compilePercentage ::
  ann ->
  ann ->
  GenLocated ann (RationalExpression ann) ->
  Validation (CompileError ann) (GenLocated ann (Ratio Natural))
compilePercentage pl pel ldl = do
  Located rl r <-
    mapValidationFailure
      ( \case
          CompileErrorInvalidRational _ lre -> CompileErrorInvalidPercentage pl pel lre
          e -> e
      )
      $ compileRational pl ldl
  pure $ Located rl $ r / 100 -- We undo the percentage-ness here

compileRational ::
  ann ->
  GenLocated ann (RationalExpression ann) ->
  Validation (CompileError ann) (GenLocated ann (Ratio Natural))
compileRational l lre@(Located rel re) = case re of
  RationalExpressionDecimal (Located del dl) ->
    case DecimalLiteral.toRatio dl of
      Nothing -> validationFailure $ CompileErrorInvalidRational l lre
      Just r -> pure (Located del r)
  RationalExpressionFraction (Located _ ndl) (Located _ ddl) ->
    case (,) <$> DecimalLiteral.toRatio ndl <*> DecimalLiteral.toRatio ddl of
      Nothing -> validationFailure $ CompileErrorInvalidRational l lre
      Just (n, d) -> do
        when (d == 0) $ validationFailure $ CompileErrorInvalidRational l lre
        pure (Located rel (n / d)) -- TODO err on zero denominator

compileTransaction ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map AccountName (GenLocated ann AccountType) ->
  GenLocated ann (Module.Transaction ann) ->
  Validation
    (CompileError ann)
    ( GenLocated ann (Ledger.Transaction ann),
      [GenLocated ann (Ledger.Price ann)]
    )
compileTransaction currencies accounts (Located l mt) = do
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
  postings <-
    traverse
      (compilePosting currencies accounts l)
      (Module.transactionPostings mt)
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
  let transactionAttachments =
        V.fromList $
          mapMaybe
            ( ( \case
                  TransactionAttachment a -> Just a
                  TransactionAssertion _ -> Nothing
              )
                . locatedValue
            )
            (Module.transactionExtras mt)
  pure
    ( Located l Ledger.Transaction {..},
      prices
    )

compilePosting ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map AccountName (GenLocated ann AccountType) ->
  ann ->
  GenLocated ann (Module.Posting ann) ->
  Validation
    (CompileError ann)
    (GenLocated ann (Ledger.Posting ann))
compilePosting currencies accounts tl (Located l mp) = do
  let postingReal = Module.postingReal mp
  -- To make sure the account is declared.
  _ <- compileAccountName accounts tl $ Module.postingAccountName mp
  let postingAccountName = Module.postingAccountName mp

  postingCurrency <- compileCurrencyDeclarationSymbol currencies tl (Module.postingCurrencySymbol mp)
  let lqf = currencyQuantisationFactor (locatedValue postingCurrency)
  postingAccount <- compileDecimalLiteral tl lqf (Module.postingAccount mp)
  postingCost <- mapM (compileCostExpression currencies tl) (Module.postingCost mp)
  postingPercentage <- mapM (compilePercentageExpression tl) (Module.postingPercentage mp)
  pure (Located l Ledger.Posting {..})

compileAccountName ::
  Map AccountName (GenLocated ann AccountType) ->
  ann ->
  GenLocated ann Module.AccountName ->
  Validation (CompileError ann) (GenLocated ann AccountType)
compileAccountName accounts tl lan@(Located _ an) =
  case M.lookup an accounts of
    Just at -> pure at
    Nothing -> validationFailure $ CompileErrorMissingAccount tl lan

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
