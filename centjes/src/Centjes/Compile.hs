{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Compile
  ( CompileError (..),
    compileDeclarations,
    compileDeclarationsCurrencies,
    compileCurrencyDeclaration,
    compilePriceDeclaration,
    compileTransaction,
    compilePosting,
  )
where

import qualified Centjes.AccountName as AccountName
import qualified Centjes.AccountType as AccountType
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format
import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Module as Module
import qualified Centjes.Tag as Tag
import Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Function
import Data.List (intercalate, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import Data.Validity (Validity)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.Amount (Rounding (..))
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
  | CompileErrorAccountCurrencyDeclaredTwice !ann !ann !CurrencySymbol
  | CompileErrorMissingTag !ann !(GenLocated ann Tag)
  | CompileErrorTagDeclaredTwice !ann !ann !Tag
  | CompileErrorCouldNotInferAccountType !ann !(GenLocated ann AccountName)
  | CompileErrorInvalidAccountCurrency !ann !ann !ann !ann !(GenLocated ann (Currency ann)) !(Set (Currency ann))
  | CompileErrorCostSameCurrency !ann !ann !ann
  | CompileErrorPriceSameCurrency !ann !ann !ann
  | CompileErrorInvalidPrice !ann !(GenLocated ann (RationalExpression ann))
  | CompileErrorInvalidPercentage !ann !ann !(GenLocated ann (RationalExpression ann))
  | CompileErrorInvalidRational !ann !(GenLocated ann (RationalExpression ann))
  | CompileErrorUnparseableAmount !ann !(GenLocated ann QuantisationFactor) !(GenLocated ann DecimalLiteral)
  deriving (Show, Generic)

instance (Validity ann, Ord ann) => Validity (CompileError ann)

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
                                Just _ -> Nothing,
                              accountDeclarationAssertions = []
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
    CompileErrorAccountCurrencyDeclaredTwice cl1 cl2 symbol ->
      Err
        (Just "CE_DUPLICATE_ACCOUNT_CURRENCY")
        (unwords ["Account assertion has duplicate currency assertion for:", show (currencySymbolText symbol)])
        [ (toDiagnosePosition cl1, Where "This currency has been asserted here first"),
          (toDiagnosePosition cl2, This "This currency has been asserted twice")
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
    CompileErrorInvalidAccountCurrency tl pl al adl (Located cl _) allowedCurrencies ->
      Err
        (Just "CE_INVALID_ACCOUNT_CURRENCY")
        ""
        [ (toDiagnosePosition adl, Where "Based on this account declaration"),
          (toDiagnosePosition cl, This "this currency is not allowed"),
          (toDiagnosePosition al, This "in this account."),
          (toDiagnosePosition pl, Where "While trying to compile this posting"),
          (toDiagnosePosition tl, Where "In this transaction")
        ]
        [ Hint $
            case map (CurrencySymbol.toString . currencySymbol) $ S.toList allowedCurrencies of
              [cur] -> unwords ["Only", cur, "is allowed."]
              curs ->
                unwords
                  [ "Only these currencies are allowed: ",
                    show curs
                  ]
        ]
    CompileErrorCostSameCurrency pl pcl ccl ->
      Err
        (Just "CE_COST_SAME_CURRENCY")
        ""
        [ (toDiagnosePosition pcl, This "This currency ..."),
          (toDiagnosePosition ccl, This "... is the same as this currency"),
          (toDiagnosePosition pl, Where "While trying to compile this posting")
        ]
        []
    CompileErrorPriceSameCurrency pdl dcl ccl ->
      Err
        (Just "CE_PRICE_SAME_CURRENCY")
        ""
        [ (toDiagnosePosition dcl, This "This currency ..."),
          (toDiagnosePosition ccl, This "... is the same as this currency"),
          (toDiagnosePosition pdl, Where "While trying to compile this price declaration")
        ]
        []
    CompileErrorMissingTag dl (Located anl t) ->
      Err
        (Just "CE_UNDECLARED_TAG")
        ( unwords
            [ "Undeclared tag:",
              Tag.toString t
            ]
        )
        [ (toDiagnosePosition dl, Where "While trying to compile this declaration"),
          (toDiagnosePosition anl, This "This tag is never declared.")
        ]
        [ Hint $
            unlines'
              [ "You can declare this tag with a tag declaration:",
                T.unpack $
                  T.strip $
                    formatDeclaration $
                      DeclarationTag $
                        noLoc $
                          TagDeclaration {tagDeclarationTag = noLoc t}
              ]
        ]
    CompileErrorTagDeclaredTwice tl1 tl2 t ->
      Err
        (Just "CE_DUPLICATE_TAG")
        (unwords ["Tag has been declared twice:", Tag.toString t])
        [ (toDiagnosePosition tl1, Where "This tag has been declared here first"),
          (toDiagnosePosition tl2, This "This tag has been declared twice")
        ]
        []
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

compileDeclarations ::
  (Ord ann) =>
  [GenLocated ann (Declaration ann)] ->
  Validation (CompileError ann) (Ledger ann)
compileDeclarations declarations = do
  let Declarations {..} = splitDeclarations declarations
  ledgerCurrencies <- compileCurrencyDeclarations declarationsCurrencies
  ledgerAccounts <- compileAccountDeclarations ledgerCurrencies declarationsAccounts
  ledgerTags <- compileTagDeclarations declarationsTags
  declarationPrices <- compilePriceDeclarations ledgerCurrencies declarationsPrices
  transactionTups <-
    traverse
      (compileTransaction ledgerCurrencies ledgerAccounts ledgerTags)
      declarationsTransactions
  let ledgerTransactions =
        V.fromList . sortOnTimestamp Ledger.transactionTimestamp $
          map fst transactionTups
  let transactionPrices = concatMap snd transactionTups
  let ledgerPrices =
        V.fromList $
          sortOnTimestamp Ledger.priceTimestamp $
            declarationPrices ++ transactionPrices
  pure Ledger {..}

data Declarations ann = Declarations
  { declarationsCurrencies :: ![GenLocated ann (CurrencyDeclaration ann)],
    declarationsAccounts :: ![GenLocated ann (AccountDeclaration ann)],
    declarationsTags :: ![GenLocated ann (TagDeclaration ann)],
    declarationsPrices :: ![GenLocated ann (PriceDeclaration ann)],
    declarationsTransactions :: ![GenLocated ann (Module.Transaction ann)]
  }

splitDeclarations ::
  [GenLocated ann (Declaration ann)] -> Declarations ann
splitDeclarations = \case
  [] -> Declarations [] [] [] [] []
  (Located _ d : ds) ->
    let tup@(Declarations cds ads tds pds ts) = splitDeclarations ds
     in case d of
          DeclarationComment _ -> tup
          DeclarationCurrency c -> Declarations (c : cds) ads tds pds ts
          DeclarationAccount a -> Declarations cds (a : ads) tds pds ts
          DeclarationTag t -> Declarations cds ads (t : tds) pds ts
          DeclarationPrice p -> Declarations cds ads tds (p : pds) ts
          DeclarationTransaction t -> Declarations cds ads tds pds (t : ts)

sortOnTimestamp :: (a -> GenLocated ann Timestamp) -> [GenLocated ann a] -> [GenLocated ann a]
sortOnTimestamp getTimestamp =
  sortBy
    ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
        `on` locatedValue
          . getTimestamp
          . locatedValue
    )

compileDeclarationsCurrencies ::
  [GenLocated ann (Declaration ann)] ->
  Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
compileDeclarationsCurrencies =
  compileCurrencyDeclarations
    . mapMaybe
      ( \case
          Located _ (DeclarationCurrency cd) -> Just cd
          _ -> Nothing
      )

compileCurrencyDeclarations ::
  [GenLocated ann (CurrencyDeclaration ann)] ->
  Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
compileCurrencyDeclarations cds = do
  tups <- traverse compileCurrencyDeclaration cds
  foldM go M.empty tups
  where
    go ::
      Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
      (CurrencySymbol, GenLocated ann QuantisationFactor) ->
      Validation (CompileError ann) (Map CurrencySymbol (GenLocated ann QuantisationFactor))
    go m (symbol, lqf@(Located l2 _)) = case M.lookup symbol m of
      Nothing -> pure $ M.insert symbol lqf m
      Just (Located l1 _) -> validationFailure $ CompileErrorCurrencyDeclaredTwice l1 l2 symbol

-- Prefer compileCurrencyDeclarations
compileCurrencyDeclaration :: GenLocated ann (CurrencyDeclaration ann) -> Validation (CompileError ann) (CurrencySymbol, GenLocated ann QuantisationFactor)
compileCurrencyDeclaration (Located l CurrencyDeclaration {..}) = do
  let Located _ symbol = currencyDeclarationSymbol
  let Located _ dl = currencyDeclarationQuantisationFactor
  qf <- case QuantisationFactor.fromDecimalLiteral dl of
    Nothing -> validationFailure $ CompileErrorInvalidQuantisationFactor l symbol currencyDeclarationQuantisationFactor
    Just qf -> pure qf
  pure (symbol, Located l qf)

compileAccountDeclarations ::
  (Ord ann) =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  [GenLocated ann (AccountDeclaration ann)] ->
  Validation (CompileError ann) (Map AccountName (GenLocated ann (Account ann)))
compileAccountDeclarations currencies ads = do
  tups <- traverse (compileAccountDeclaration currencies) ads
  foldM go M.empty tups
  where
    go ::
      Map AccountName (GenLocated ann (Account ann)) ->
      (AccountName, GenLocated ann (Account ann)) ->
      Validation (CompileError ann) (Map AccountName (GenLocated ann (Account ann)))
    go m (an, lat@(Located l2 _)) = case M.lookup an m of
      Nothing -> pure $ M.insert an lat m
      Just (Located l1 _) -> validationFailure $ CompileErrorAccountDeclaredTwice l1 l2 an

compileAccountDeclaration ::
  forall ann.
  (Ord ann) =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (AccountDeclaration ann) ->
  Validation (CompileError ann) (AccountName, GenLocated ann (Account ann))
compileAccountDeclaration currencies (Located dl AccountDeclaration {..}) = do
  let Located _ name = accountDeclarationName
  accountType <- case accountDeclarationType of
    Just (Located _ t) -> pure t
    Nothing -> case AccountType.fromAccountName name of
      Just t -> pure t
      Nothing -> validationFailure $ CompileErrorCouldNotInferAccountType dl accountDeclarationName
  currencyAssertions <- traverse (compileAccountAssertionCurrency currencies) accountDeclarationAssertions
  accountCurrencies <-
    if null currencyAssertions
      then pure Nothing
      else Just . M.keysSet <$> foldM goCurrency M.empty currencyAssertions
  pure (name, Located dl Account {..})
  where
    goCurrency ::
      Map (Currency ann) ann ->
      GenLocated ann (Currency ann) ->
      Validation (CompileError ann) (Map (Currency ann) ann)
    goCurrency assertedCurrencies (Located l2 cur) =
      case M.lookup cur assertedCurrencies of
        Just l1 -> validationFailure $ CompileErrorAccountCurrencyDeclaredTwice l1 l2 (currencySymbol cur)
        Nothing -> pure $ M.insert cur l2 assertedCurrencies

compileAccountAssertionCurrency ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (AccountAssertion ann) ->
  Validation (CompileError ann) (GenLocated ann (Currency ann))
compileAccountAssertionCurrency currencies (Located al (AccountAssertionCurrency lcs)) = do
  Located _ cur <- compileCurrencySymbol currencies al lcs
  pure $ Located al cur

compileTagDeclarations ::
  [GenLocated ann (TagDeclaration ann)] ->
  Validation (CompileError ann) (Map Tag ann)
compileTagDeclarations tds = do
  tups <- traverse compileTagDeclaration tds
  foldM go M.empty tups
  where
    go ::
      Map Tag ann ->
      (Tag, ann) ->
      Validation (CompileError ann) (Map Tag ann)
    go m (an, l2) = case M.lookup an m of
      Nothing -> pure $ M.insert an l2 m
      Just l1 -> validationFailure $ CompileErrorTagDeclaredTwice l1 l2 an

compileTagDeclaration ::
  GenLocated ann (TagDeclaration ann) ->
  Validation (CompileError ann) (Tag, ann)
compileTagDeclaration (Located dl TagDeclaration {..}) = do
  let Located _ tag = tagDeclarationTag
  pure (tag, dl)

compilePriceDeclarations ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  [GenLocated ann (PriceDeclaration ann)] ->
  Validation (CompileError ann) [GenLocated ann (Price ann)]
compilePriceDeclarations currencies = traverse (compilePriceDeclaration currencies)

compilePriceDeclaration ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (PriceDeclaration ann) ->
  Validation (CompileError ann) (GenLocated ann (Price ann))
compilePriceDeclaration currencies (Located pdl PriceDeclaration {..}) = do
  let priceTimestamp = priceDeclarationTimestamp
  priceCurrency <- compileCurrencySymbol currencies pdl priceDeclarationCurrencySymbol
  priceCost <- compileCostExpression currencies pdl priceDeclarationCost
  do
    let Located dcl pCur = priceCurrency
    let Located ccl cCur = costCurrency (locatedValue priceCost)
    when
      ( currencySymbol pCur
          == currencySymbol cCur
      )
      $ validationFailure
      $ CompileErrorPriceSameCurrency pdl dcl ccl
  pure $ Located pdl Price {..}

compileCostExpression ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (CostExpression ann) ->
  Validation (CompileError ann) (GenLocated ann (Cost ann))
compileCostExpression currencies pdl (Located cl CostExpression {..}) = do
  costConversionRate <- compileConversionRate pdl costExpressionConversionRate
  costCurrency <- compileCurrencySymbol currencies pdl costExpressionCurrencySymbol
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
  let percentageInclusive = fromMaybe True percentageExpressionInclusive
  let percentageRounding = fromMaybe RoundNearest percentageExpressionRounding
  percentageRatio <- compilePercentage pl pel percentageExpressionRationalExpression
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
  (Ord ann) =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map AccountName (GenLocated ann (Account ann)) ->
  Map Tag ann ->
  GenLocated ann (Module.Transaction ann) ->
  Validation
    (CompileError ann)
    ( GenLocated ann (Ledger.Transaction ann),
      [GenLocated ann (Ledger.Price ann)]
    )
compileTransaction currencies accounts tags (Located l mt) = do
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
  Extras {..} <- compileExtras currencies tags l (Module.transactionExtras mt)
  let transactionAttachments = V.fromList $ DList.toList extraAttachments
  let transactionAssertions = V.fromList $ DList.toList extraAssertions
  let transactionTags = extraTags
  pure
    ( Located l Ledger.Transaction {..},
      prices
    )

data Extras ann = Extras
  { extraAttachments :: !(DList (GenLocated ann (Ledger.Attachment ann))),
    extraAssertions :: !(DList (GenLocated ann (Ledger.Assertion ann))),
    extraTags :: !(Map Tag ann)
  }

-- We need this semigroup instance to show as many errors as possible.
instance Semigroup (Extras ann) where
  (<>) e1 e2 =
    Extras
      { extraAttachments = extraAttachments e1 <> extraAttachments e2,
        extraAssertions = extraAssertions e1 <> extraAssertions e2,
        extraTags = M.union (extraTags e1) (extraTags e2)
      }

instance Monoid (Extras ann) where
  mempty =
    Extras
      { extraAttachments = mempty,
        extraAssertions = mempty,
        extraTags = M.empty
      }
  mappend = (<>)

compileExtras ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map Tag ann ->
  ann ->
  [GenLocated ann (TransactionExtra ann)] ->
  Validation
    (CompileError ann)
    (Extras ann)
compileExtras currencies tags l = foldMap $ compileExtra currencies tags l

compileExtra ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map Tag ann ->
  ann ->
  GenLocated ann (TransactionExtra ann) ->
  Validation
    (CompileError ann)
    (Extras ann)
compileExtra currencies tags l (Located _ e) = case e of
  TransactionAssertion a ->
    (\ca -> mempty {extraAssertions = DList.singleton ca})
      <$> compileAssertion currencies l a
  TransactionAttachment (Located _ (ExtraAttachment a)) ->
    pure $ mempty {extraAttachments = DList.singleton a}
  TransactionTag et ->
    (\ct -> mempty {extraTags = uncurry M.singleton ct})
      <$> compileTag tags l et

compilePosting ::
  (Ord ann) =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map AccountName (GenLocated ann (Account ann)) ->
  ann ->
  GenLocated ann (Module.Posting ann) ->
  Validation
    (CompileError ann)
    (GenLocated ann (Ledger.Posting ann))
compilePosting currencies accounts tl (Located l mp) = do
  let postingReal = Module.postingReal mp

  let postingAccountName@(Located al _) = Module.postingAccountName mp
  account <- compileAccountName accounts tl postingAccountName

  postingCurrency <- compileCurrencySymbol currencies tl (Module.postingCurrencySymbol mp)
  checkAccountCurrencyAssertion tl l al account postingCurrency

  let lqf = currencyQuantisationFactor (locatedValue postingCurrency)
  postingAccount <- compileDecimalLiteral tl lqf (Module.postingAccount mp)
  postingCost <- for (Module.postingCost mp) $ \ce -> do
    lCost@(Located _ cost) <- compileCostExpression currencies tl ce
    let Located pcl pCur = postingCurrency
    let Located ccl cCur = costCurrency cost
    when
      ( currencySymbol pCur
          == currencySymbol cCur
      )
      $ validationFailure
      $ CompileErrorCostSameCurrency tl pcl ccl
    pure lCost
  postingPercentage <- traverse (compilePercentageExpression tl) (Module.postingPercentage mp)
  pure (Located l Ledger.Posting {..})

compileAccountName ::
  Map AccountName (GenLocated ann (Account ann)) ->
  ann ->
  GenLocated ann Module.AccountName ->
  Validation (CompileError ann) (GenLocated ann (Account ann))
compileAccountName accounts tl lan@(Located _ an) =
  case M.lookup an accounts of
    Just (Located l acc) -> pure (Located l acc)
    Nothing -> validationFailure $ CompileErrorMissingAccount tl lan

checkAccountCurrencyAssertion ::
  (Ord ann) =>
  ann ->
  ann ->
  ann ->
  GenLocated ann (Account ann) ->
  GenLocated ann (Currency ann) ->
  Validation (CompileError ann) ()
checkAccountCurrencyAssertion tl pl al (Located adl Account {..}) lcur@(Located _ cur) =
  case accountCurrencies of
    Nothing -> pure ()
    Just allowedCurrencies ->
      if S.member cur allowedCurrencies
        then pure ()
        else validationFailure $ CompileErrorInvalidAccountCurrency tl pl al adl lcur allowedCurrencies

compileAssertion ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  ann ->
  GenLocated ann (Module.ExtraAssertion ann) ->
  Validation (CompileError ann) (GenLocated ann (Ledger.Assertion ann))
compileAssertion currencies tl (Located l (ExtraAssertion (Located _ (Module.AssertionEquals lan ldl lqs)))) = do
  lc <- compileCurrencySymbol currencies tl lqs
  let lqf = currencyQuantisationFactor (locatedValue lc)
  la <- compileDecimalLiteral tl lqf ldl
  pure (Located l (Ledger.AssertionEquals lan la lc))

compileTag ::
  Map Tag ann ->
  ann ->
  GenLocated ann (ExtraTag ann) ->
  Validation (CompileError ann) (Tag, ann)
compileTag tags tl (Located etl (ExtraTag lt@(Located _ tag))) =
  case M.lookup tag tags of
    Nothing -> validationFailure $ CompileErrorMissingTag tl lt
    Just _ -> pure (tag, etl)

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
compileDecimalLiteral tl lqf@(Located _ qf) ldl@(Located ll dl) = case Account.fromDecimalLiteral qf dl of
  Nothing -> validationFailure $ CompileErrorUnparseableAmount tl lqf ldl
  Just a -> pure (Located ll a)
