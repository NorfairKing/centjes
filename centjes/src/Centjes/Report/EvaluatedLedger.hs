{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.EvaluatedLedger
  ( EvaluatedLedger (..),
    EvaluatedEntry (..),
    EvaluatedTransaction (..),
    EvaluatedPosting (..),
    EvaluatedPrice (..),
    EvaluatedLedgerError (..),
    AccountBalances,
    BalanceError (..),
    produceEvaluatedLedger,
    checkEvaluatedLedgerAssertions,
    balanceTransaction,
    filterAccountBalances,
    convertAccountBalances,
    fillAccountBalances,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.AccountType as AccountType
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Convert.PriceGraph (PriceGraph)
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Centjes.CurrencySymbol as CurrencySymbol
import qualified Centjes.Filter as Filter
import Centjes.Format
import Centjes.Ledger
import Centjes.Location
import Centjes.Module (RationalExpression (..))
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.Foldable (for_, traverse_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Data.Validity hiding (Validation (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import qualified Numeric.DecimalLiteral as DecimalLiteral

type AccountBalances ann = Map AccountName (Money.MultiAccount (Currency ann))

data EvaluatedLedger ann = EvaluatedLedger
  { evaluatedLedgerSource :: !(Ledger ann),
    evaluatedLedgerEntries :: !(Vector (EvaluatedEntry ann))
  }

data EvaluatedEntry ann
  = EvaluatedEntryTransaction !(EvaluatedTransaction ann)
  | EvaluatedEntryPrice !(EvaluatedPrice ann)

data EvaluatedTransaction ann = EvaluatedTransaction
  { evaluatedTransactionLocated :: !(GenLocated ann (Transaction ann)),
    evaluatedTransactionPostings :: Vector (EvaluatedPosting ann),
    -- | Cumulative account balances (all accounts) including virtual postings,
    -- right after this transaction has been incorporated
    evaluatedTransactionBalancesWithVirtual :: !(AccountBalances ann),
    -- | Cumulative account balances (all accounts) excluding virtual postings,
    -- right after this transaction has been incorporated
    evaluatedTransactionBalancesWithoutVirtual :: !(AccountBalances ann),
    -- | The cumulative price graph at the time of this transaction
    evaluatedTransactionPriceGraph :: !(MemoisedPriceGraph (Currency ann))
  }

data EvaluatedPosting ann = EvaluatedPosting
  { evaluatedPostingLocated :: !(GenLocated ann (Posting ann)),
    -- | The posting amount (for this posting alone)
    evaluatedPostingAmount :: !(Money.MultiAccount (Currency ann)),
    -- | Cumulative account balances (all accounts) including virtual postings,
    -- right after this posting has been incorporated
    evaluatedPostingBalancesWithVirtual :: !(AccountBalances ann),
    -- | Cumulative account balances (all accounts) excluding virtual postings,
    -- right after this posting has been incorporated
    evaluatedPostingBalancesWithoutVirtual :: !(AccountBalances ann),
    -- | The cumulative price graph at the time of this posting
    evaluatedPostingPriceGraph :: !(MemoisedPriceGraph (Currency ann))
  }

data EvaluatedPrice ann = EvaluatedPrice
  { evaluatedPriceLocated :: !(GenLocated ann (Price ann)),
    -- | The cumulative account balances at this point (unchanged from
    -- the previous entry, since a price doesn't affect balances)
    evaluatedPriceBalancesWithVirtual :: !(AccountBalances ann),
    evaluatedPriceBalancesWithoutVirtual :: !(AccountBalances ann),
    -- | The cumulative price graph right after this price declaration
    evaluatedPricePriceGraph :: !(MemoisedPriceGraph (Currency ann))
  }

data EvaluatedLedgerError ann
  = -- | Error from per-transaction validation (balancing, cost conversion, ratios)
    EvaluatedLedgerErrorBalanceError !(BalanceError ann)
  | -- | Could not add transaction to running total balance
    -- [tag:EVAL_RUNNING_BALANCE]
    EvaluatedLedgerErrorCouldNotAddTransaction !ann !AccountName !(Money.MultiAccount (Currency ann)) !(Money.MultiAccount (Currency ann))
  | -- | Could not add posting to account balances
    -- [tag:EVAL_ACCOUNT_TOTAL]
    EvaluatedLedgerErrorCouldNotAddPostings !ann !AccountName !ann !(Money.MultiAccount (Currency ann)) !(Currency ann) !Money.Account
  | -- | Undeclared account encountered during assertion
    -- [tag:EVAL_UNDECLARED_ACCOUNT]
    EvaluatedLedgerErrorUndeclaredAccount !ann !AccountName
  | -- | Account type assertion failure
    -- [tag:EVAL_ACCOUNT_TYPE_ASSERTION]
    EvaluatedLedgerErrorAccountTypeAssertion !ann !ann !AccountType !(Money.MultiAccount (Currency ann))
  | -- | Explicit assertion failure
    -- [tag:EVAL_ASSERTION]
    EvaluatedLedgerErrorAssertion !ann !(GenLocated ann (Assertion ann)) !(Money.MultiAccount (Currency ann)) !(Maybe Money.Account)
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (EvaluatedLedgerError ann)

instance ToReport (EvaluatedLedgerError SourceSpan) where
  toReport = \case
    EvaluatedLedgerErrorBalanceError be -> toReport be
    -- [tag:EVAL_RUNNING_BALANCE] At least one test per error: test_resources/balance/error/EVAL_RUNNING_BALANCE.cent
    EvaluatedLedgerErrorCouldNotAddTransaction s an subtotal current ->
      Err
        (Just "EVAL_RUNNING_BALANCE")
        "Could not add transaction to the running total balance"
        [ (toDiagnosePosition s, Where "While trying to incorporate this transaction into the running balance"),
          (toDiagnosePosition s, This "Could not add the balance of this transaction to the running total."),
          ( toDiagnosePosition s,
            Where
              ( unlines' $
                  concat
                    [ [unwords ["Account: ", AccountName.toString an]],
                      ["Running total:"],
                      multiAccountLines subtotal,
                      ["Balance to add:"],
                      multiAccountLines current
                    ]
              )
          )
        ]
        []
    -- [tag:EVAL_ACCOUNT_TOTAL] At least one test per error: test_resources/balance/error/EVAL_ACCOUNT_TOTAL.cent
    EvaluatedLedgerErrorCouldNotAddPostings st an sp runningTotal c a ->
      Err
        (Just "EVAL_ACCOUNT_TOTAL")
        "Could not add postings to compute the total amount"
        [ (toDiagnosePosition st, Where "While trying to balance this transaction"),
          (toDiagnosePosition st, This "Could not add these amounts together because the result would become too big:"),
          ( toDiagnosePosition st,
            Where $
              unlines' $
                concat
                  [ [unwords ["Account: ", AccountName.toString an]],
                    ["Running total:"],
                    multiAccountLines runningTotal
                  ]
          ),
          (toDiagnosePosition sp, Where "while trying to incorporate this posting"),
          ( toDiagnosePosition sp,
            Where $
              unlines'
                ["Amount to add:", accountLine c a]
          )
        ]
        []
    -- [tag:EVAL_UNDECLARED_ACCOUNT] Internal error, should never trigger past compilation
    EvaluatedLedgerErrorUndeclaredAccount s an ->
      Err
        (Just "EVAL_UNDECLARED_ACCOUNT")
        (unwords ["This account has not been declared:", AccountName.toString an])
        [(toDiagnosePosition s, Where "While trying to balance this transaction")]
        [Note "This is an internal exception and should never trigger past compilation"]
    -- [tag:EVAL_ACCOUNT_TYPE_ASSERTION] At least one test per error: test_resources/balance/error/EVAL_ACCOUNT_TYPE_ASSERTION.cent
    EvaluatedLedgerErrorAccountTypeAssertion s adl at bal ->
      Err
        (Just "EVAL_ACCOUNT_TYPE_ASSERTION")
        "Balance of an account did not match its type"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          ( toDiagnosePosition adl,
            Where $
              unlines'
                [ unwords ["Based on this account and the account type", AccountType.toString at <> ", "],
                  unwords
                    [ "Which implies that the balance must be",
                      case at of
                        AccountTypeAssets -> "positive"
                        AccountTypeLiabilities -> "negative"
                        AccountTypeEquity -> "negative"
                        AccountTypeExpenses -> "positive"
                        AccountTypeIncome -> "negative"
                        AccountTypeOther -> "any number" -- Unused
                    ]
                ]
          ),
          (toDiagnosePosition s, This $ unlines' ("The balance was" : multiAccountLines bal))
        ]
        []
    -- [tag:EVAL_ASSERTION] At least one test per error: test_resources/balance/error/EVAL_ASSERTION.cent, EVAL_ASSERTION-wrong-currency.cent
    EvaluatedLedgerErrorAssertion s (Located al (AssertionEquals _ (Located _ asserted) (Located _ c))) actual mDifference ->
      Err
        (Just "EVAL_ASSERTION")
        "Assertion failure"
        ( concat
            [ [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
                (toDiagnosePosition al, This "This assertion failed"),
                ( toDiagnosePosition s,
                  Where $
                    unlines' $
                      concat $
                        [ ["Calculated:"],
                          multiAccountLines actual,
                          ["Asserted:", accountLine c asserted]
                        ]
                          ++ ( [ [ "Difference (calculated - asserted):",
                                   accountLine c difference
                                 ]
                               | difference <- maybeToList mDifference
                               ]
                             )
                )
              ],
              [ ( toDiagnosePosition s,
                  Maybe $
                    unlines'
                      [ "The actual balance has no amount in the asserted currency.",
                        "Maybe there is a mistake with the currency?"
                      ]
                )
              | not (M.member c (MultiAccount.unMultiAccount actual))
              ]
            ]
        )
        []

data BalanceError ann
  = BalanceErrorCouldNotAddPostings !ann !AccountName !ann !(Money.MultiAccount (Currency ann)) !(Currency ann) !Money.Account
  | BalanceErrorCouldNotSumPostings !ann ![Money.MultiAccount (Currency ann)]
  | BalanceErrorConversionTooBig !(GenLocated ann Money.Account) !(GenLocated ann (Cost ann))
  | BalanceErrorConversionImpossibleRate !(GenLocated ann Money.Account) !(GenLocated ann (Cost ann)) !(Maybe Money.ConversionRate)
  | BalanceErrorTransactionOffBalance !ann !(Money.MultiAccount (Currency ann)) ![GenLocated ann (Posting ann)]
  | BalanceErrorAmountRatioNoPrevious !ann !ann
  | BalanceErrorAmountRatioCurrency !ann !ann !ann
  | BalanceErrorAmountRatioFraction !ann !ann !ann !Bool
  | BalanceErrorAmountRatioFraction' !ann !ann !Rational
  | BalanceErrorAmountRatio !ann !ann !ann !ann !(Currency ann) !(GenLocated ann Money.Account) !(Maybe (GenLocated ann Money.Account)) !(Maybe (GenLocated ann Rational))
  | BalanceErrorConvertError !(ConvertError ann)
  | BalanceErrorCouldNotFill
  | BalanceErrorCouldNotSumTotal ![Money.MultiAccount (Currency ann)]
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceError ann)

instance ToReport (BalanceError SourceSpan) where
  toReport = \case
    -- [tag:BE_ACCOUNT_TOTAL] At least one test per error: test_resources/balance/error/BE_ACCOUNT_TOTAL.cent
    BalanceErrorCouldNotAddPostings st an sp runningTotal c a ->
      Err
        (Just "BE_ACCOUNT_TOTAL")
        "Could not add postings to compute the total amount"
        [ (toDiagnosePosition st, Where "While trying to balance this transaction"),
          (toDiagnosePosition st, This "Could not add these amounts together because the result would become too big:"),
          ( toDiagnosePosition st,
            Where $
              unlines' $
                concat
                  [ [unwords ["Account: ", AccountName.toString an]],
                    ["Running total:"],
                    multiAccountLines runningTotal
                  ]
          ),
          (toDiagnosePosition sp, Where "while trying to incorporate this posting"),
          ( toDiagnosePosition sp,
            Where $
              unlines'
                ["Amount to add:", accountLine c a]
          )
        ]
        []
    -- [tag:BE_TRANSACTION_SUM] At least one test per error: test_resources/balance/error/BE_TRANSACTION_SUM.cent
    BalanceErrorCouldNotSumPostings s mas ->
      Err
        (Just "BE_TRANSACTION_SUM")
        "Could not sum postings within transaction"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition s, This "Could not sum these amounts together because the result would become too big:"),
          (toDiagnosePosition s, Where $ unlines' $ concatMap multiAccountLines mas)
        ]
        []
    -- [tag:BE_CONVERSION_TOO_BIG] At least one test per error: test_resources/balance/error/BE_CONVERSION_TOO_BIG.cent
    BalanceErrorConversionTooBig (Located al _) (Located cl _) ->
      Err
        (Just "BE_CONVERSION_TOO_BIG")
        "Could not convert amount because it got too big"
        [ (toDiagnosePosition cl, Where "Using this cost"),
          (toDiagnosePosition al, This "Could not convert this amount")
        ]
        []
    -- [tag:BE_CONVERSION_IMPOSSIBLE_RATE] At least one test per error: test_resources/balance/error/conversion-impossible-rate.cent
    BalanceErrorConversionImpossibleRate (Located al _) (Located cl _) mConversionRate ->
      Err
        (Just "BE_CONVERSION_IMPOSSIBLE_RATE")
        "Could not convert amount because the rate made it impossible to convert to an integer number of minimal quantisations."
        [ (toDiagnosePosition cl, Where "Using this cost"),
          (toDiagnosePosition al, This "Could not convert this amount")
        ]
        [ Hint $ "The closest valid rate is " <> DecimalLiteral.toString dl
        | cr <- maybeToList mConversionRate,
          dl <- maybeToList $ ConversionRate.toDecimalLiteral cr
        ]
    -- [tag:BE_OFF_BALANCE] At least one test per error: test_resources/balance/error/BE_OFF_BALANCE-*.cent
    BalanceErrorTransactionOffBalance s ma postings ->
      Err
        (Just "BE_OFF_BALANCE")
        "Could not balance transaction"
        ( [ (toDiagnosePosition s, This "This transaction is off balance."),
            (toDiagnosePosition s, Where $ unlines' ("By this amount:" : multiAccountLines ma))
          ]
            ++ mapMaybe
              (\(Located _ Posting {..}) -> makePostingSuggestion ma postingCurrency postingAccount)
              postings
        )
        []
    -- [tag:BE_AMOUNT_RATIO_NO_PREVIOUS] At least one test per error: test_resources/balance/error/BE_PERCENTAGE_NO_PREVIOUS.cent
    BalanceErrorAmountRatioNoPrevious s pctl ->
      Err
        (Just "BE_AMOUNT_RATIO_NO_PREVIOUS")
        "Posting with a ratio has no previous posting"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition pctl, This "This ratio requires a previous posting")
        ]
        []
    -- [tag:BE_AMOUNT_RATIO_CURRENCY] At least one test per error: test_resources/balance/error/BE_PERCENTAGE_CURRENCY.cent
    BalanceErrorAmountRatioCurrency s pcl cl ->
      Err
        (Just "BE_AMOUNT_RATIO_CURRENCY")
        "Posting with ratio has different currency than the previous posting"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition pcl, This "This currency is different ..."),
          (toDiagnosePosition cl, This "... from this currency")
        ]
        []
    -- [tag:BE_AMOUNT_RATIO_FRACTION] At least one test per error: test_resources/balance/error/BE_PERCENTAGE_FRACTION.cent
    BalanceErrorAmountRatioFraction s al rl isInverse ->
      Err
        (Just "BE_AMOUNT_RATIO_FRACTION")
        ""
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition al, This $ unwords ["Could not", if isInverse then "divide" else "multiply", "this amount"]),
          (toDiagnosePosition rl, This "... by this ratio because the amount would get too big.")
        ]
        []
    -- [tag:BE_AMOUNT_RATIO_FRACTION'] At least one test per error: test_resources/balance/error/BE_PERCENTAGE-zero.cent
    BalanceErrorAmountRatioFraction' s al ratio ->
      Err
        (Just "BE_AMOUNT_RATIO_FRACTION")
        ""
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          ( toDiagnosePosition al,
            This $
              unlines'
                [ "Could not multiply this amount by",
                  show ratio,
                  "because the amount would become too big"
                ]
          )
        ]
        []
    -- [tag:BE_AMOUNT_RATIO] At least one test per error: test_resources/balance/error/BE_PERCENTAGE-exclusive.cent, BE_PERCENTAGE-inclusive.cent
    BalanceErrorAmountRatio s pl ppl pctl currency (Located pal computedPrevious) mComputedCurrent mComputedRatio ->
      let Located _ qf = currencyQuantisationFactor currency
       in Err
            (Just "BE_AMOUNT_RATIO")
            "The given ratio does not match the amount it describes."
            ( concat
                [ [ (toDiagnosePosition pctl, This "Using this ratio")
                  ],
                  [ ( toDiagnosePosition rl,
                      Maybe $
                        unlines
                          [ "Perhaps this ratio needs to be",
                            T.unpack $
                              formatRationalExpression $
                                RationalExpression
                                  { rationalExpressionNumerator = noLoc dl,
                                    rationalExpressionDenominator = Nothing,
                                    rationalExpressionPercent = True
                                  }
                          ]
                    )
                  | Located rl expectedRatio <- maybeToList mComputedRatio,
                    dl <- maybeToList $ DecimalLiteral.fromRational (expectedRatio * 100)
                  ],
                  [ ( toDiagnosePosition pal,
                      Maybe $
                        unlines
                          [ "Perhaps this amount needs to be",
                            Account.format qf computedPrevious
                          ]
                    )
                  ],
                  [ ( toDiagnosePosition al,
                      Maybe $
                        unlines
                          [ "Perhaps this amount needs to be",
                            Account.format qf computedCurrent
                          ]
                    )
                  | Located al computedCurrent <- maybeToList mComputedCurrent
                  ],
                  [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
                    (toDiagnosePosition pl, Where "While checking the ratio in this posting"),
                    (toDiagnosePosition ppl, Where "Which relates to the amount in this posting")
                  ]
                ]
            )
            [Hint "Make sure to double-check the signs of the suggested and actual amounts."]
    -- [tag:BE_CONVERT_ERROR] At least one test per error: test_resources/balance/error/CONVERT_ERROR_*.cent
    BalanceErrorConvertError ce -> toReport ce
    -- [tag:BE_FILL] At least one test per error: test_resources/balance/error/BE_FILL.cent
    BalanceErrorCouldNotFill ->
      Err
        (Just "BE_FILL")
        "Could not fill accounts hierarchically because the result got too big."
        []
        []
    -- [tag:BE_TOTAL] At least one test per error: test_resources/balance/error/BE_FILL.cent (triggers both BE_FILL and BE_TOTAL)
    BalanceErrorCouldNotSumTotal _ ->
      Err
        (Just "BE_TOTAL")
        "Could not sum all amounts together because the result got too big."
        []
        []

makePostingSuggestion ::
  Money.MultiAccount (Currency SourceSpan) ->
  Located (Currency SourceSpan) ->
  Located Money.Account ->
  Maybe (Position, Marker String)
makePostingSuggestion total (Located cl currency) (Located al account) =
  let accountMap = MultiAccount.unMultiAccount total
      totalCurrencies :: Set (Currency SourceSpan)
      totalCurrencies = M.keysSet accountMap
   in if totalCurrencies /= S.singleton currency
        then Nothing
        else do
          totalAccount <- M.lookup currency accountMap
          suggestedAccount <- Account.subtract account totalAccount
          let qf = locatedValue (currencyQuantisationFactor currency)
          suggestedLiteral <- Account.toDecimalLiteral qf suggestedAccount
          let symbol = currencySymbol currency
          pure
            ( toDiagnosePosition $ combineSpans al cl,
              Maybe $
                unwords
                  [ "Perhaps you meant",
                    DecimalLiteral.toString suggestedLiteral,
                    T.unpack (currencySymbolText symbol)
                  ]
            )

balanceTransaction ::
  forall ann.
  (Ord ann) =>
  Bool ->
  GenLocated ann (Transaction ann) ->
  Validation
    (BalanceError ann)
    ( GenLocated ann (AccountBalances ann),
      GenLocated ann (AccountBalances ann)
    )
balanceTransaction showVirtual (Located tl Transaction {..}) = do
  let incorporatePosting ::
        -- (Balances for transaction balance checking, balances of acounts)
        (AccountBalances ann, AccountBalances ann, AccountBalances ann) ->
        Int ->
        GenLocated ann (Posting ann) ->
        Validation
          (BalanceError ann)
          ( AccountBalances ann,
            AccountBalances ann,
            AccountBalances ann
          )
      incorporatePosting
        (balancesForBalancing, balancesForAssertions, balancesForUser)
        ix
        (Located pl (Posting real (Located _ an) lc@(Located _ currency) la@(Located _ account) mCost mAmountRatio)) = do
          for_ mAmountRatio $ \lpct@(Located pctl _) -> do
            let go i =
                  case transactionPostings V.!? pred i of
                    Nothing -> validationFailure $ BalanceErrorAmountRatioNoPrevious tl pctl
                    Just lp@(Located _ p) ->
                      if postingReal p
                        then pure lp
                        else go (pred i)

            lp <- go ix
            checkAmountRatio tl pl lp lc la lpct

          balancesForBalancing' <-
            if real
              then do
                (convertedCurrency, convertedAccount) <- case mCost of
                  Nothing -> pure (currency, account)
                  Just lcost@(Located _ Cost {..}) -> do
                    let Located _ qf = currencyQuantisationFactor currency
                    let Located _ newCurrency = costCurrency
                    let Located _ rate = costConversionRate
                    let Located _ qfNew = currencyQuantisationFactor newCurrency
                    -- Separate the amount for balancing from the amount for adding to balance
                    let (mNewAccount, mActualRate) = Account.convert Account.RoundNearest qf account rate qfNew
                    if mActualRate == Just rate
                      then case mNewAccount of
                        Nothing -> validationFailure $ BalanceErrorConversionTooBig la lcost
                        Just newAccount -> pure (newCurrency, newAccount)
                      else validationFailure $ BalanceErrorConversionImpossibleRate la lcost mActualRate

                addAccountToBalances tl an pl convertedCurrency convertedAccount balancesForBalancing
              else pure balancesForBalancing

          balancesForAssertions' <-
            if real
              then addAccountToBalances tl an pl currency account balancesForAssertions
              else pure balancesForAssertions

          balancesForUser' <-
            if real || (not real && showVirtual)
              then addAccountToBalances tl an pl currency account balancesForUser
              else pure balancesForUser

          pure (balancesForBalancing', balancesForAssertions', balancesForUser')

  -- We keep three sets of balances:
  -- 1. For balancing the transaction, and assertions.
  --    These are
  --    - Only real postings
  --    - Amounts converted only according to costs listed in the transaction
  -- 2. For inter-transaction account assertions
  --    These are
  --    - Only real postings
  --    - Unconverted amounts
  -- 3. For showing to the user.
  --    These may include
  --    - Virtual postings (if showVirtual is True)
  --    - Unconverted amounts (according to their costs)
  --    - Converted accounts (according to whether a currency should be converted for the user.)
  (mForBalancing, mForAssertions, mForUser) <- V.ifoldM incorporatePosting (M.empty, M.empty, M.empty) transactionPostings
  let as = M.elems mForBalancing
  case MultiAccount.sum as of
    Nothing -> validationFailure $ BalanceErrorCouldNotSumPostings tl as
    Just d
      | d == MultiAccount.zero -> pure (Located tl mForAssertions, Located tl mForUser)
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance tl d $ V.toList transactionPostings

addAccountToBalances ::
  (Ord ann) =>
  ann ->
  AccountName ->
  ann ->
  Currency ann ->
  Money.Account ->
  AccountBalances ann ->
  Validation (BalanceError ann) (AccountBalances ann)
addAccountToBalances tl an pl c a bs = case M.lookup an bs of
  Nothing -> pure $ M.insert an (MultiAccount.fromAccount c a) bs
  Just a' ->
    case MultiAccount.addAccount a' c a of
      Nothing -> validationFailure $ BalanceErrorCouldNotAddPostings tl an pl a' c a
      Just a'' -> pure $ M.insert an a'' bs

checkAmountRatio ::
  (Eq ann) =>
  -- Transaction ann
  ann ->
  -- Posting ann
  ann ->
  -- Previous posting
  GenLocated ann (Posting ann) ->
  -- Current posting's currency
  GenLocated ann (Currency ann) ->
  -- Current posting's account
  GenLocated ann Money.Account ->
  -- Current posting's ratio
  GenLocated ann (AmountRatio ann) ->
  Validation (BalanceError ann) ()
checkAmountRatio
  tl
  pl
  (Located ppl p)
  (Located cl currency)
  (Located al thisAccount)
  (Located pctl (AmountRatio inclusive rounding (Located rl ratio))) = do
    let Located pcl newCurrency = postingCurrency p
    when (newCurrency /= currency) $ validationFailure $ BalanceErrorAmountRatioCurrency tl pcl cl
    let Located pal previousAccount = postingAccount p
    if inclusive
      then do
        -- T: Total amount (previous posting's amount)
        -- P: Part amount (this posting's amount)
        -- f: Fraction (The ratio)
        -- E: Exclusive (The exclusive version of T)
        --
        -- These are defined as:
        -- T = (E + E * f) and P = E * f
        --
        -- Rewritten as:
        -- T = E * (1 + f)
        -- E = T / (1 + f)
        --
        -- E = P / f
        --
        -- P / f = T / (1 + f)
        --
        -- We calculate:
        --   * P based on T and f
        --   * f based on P and T
        -- and compare those to the actual P and f.
        --
        -- We then suggest "fixed" versions of all three of the parts by computing
        -- them from the other two:
        --
        -- Each defined in terms of the other two:
        -- P = T * (f / (1 + f))
        -- T = P * ((1 + f) / f)
        -- f = P / (T / (1 + f))
        --   = (1 + f) * P / T
        let computedPRatio = ratio / (1 + ratio)
        (computedP, whereToRoundNext) <- case Account.fraction rounding previousAccount computedPRatio of
          (Nothing, _) -> validationFailure $ BalanceErrorAmountRatioFraction' tl pal computedPRatio
          (Just cP, actualRatio) ->
            pure
              ( cP,
                case compare computedPRatio actualRatio of
                  LT -> Account.RoundDown
                  EQ -> Account.RoundNearest
                  GT -> Account.RoundUp
              )
        when (computedP /= thisAccount) $ do
          mComputedT <-
            if ratio == 0
              then pure Nothing
              else do
                let computedTRatio = (1 + ratio) / ratio
                case fst $ Account.fraction whereToRoundNext thisAccount computedTRatio of
                  Nothing -> validationFailure $ BalanceErrorAmountRatioFraction' tl al computedTRatio
                  Just cT -> pure $ Just $ Located pal cT

          let mComputedRatio = do
                let Located _ qf = currencyQuantisationFactor currency
                rate <- ConversionRate.toRational <$> Account.rate qf previousAccount qf thisAccount
                pure $ Located rl $ rate * (1 + ratio)

          validationFailure $
            BalanceErrorAmountRatio
              -- Transaction location
              tl
              -- Posting location
              pl
              -- Previous posting location
              ppl
              -- AmountRatio location
              pctl
              -- Currency
              currency
              -- Computed P
              (Located al computedP)
              -- Computed T
              mComputedT
              -- Computed f
              mComputedRatio
      else do
        -- E: Exclusive (previous posting's amount)
        -- P: Part amount (this posting's amount)
        -- T: Total amount (E + P)
        -- f: Fraction (The ratio)
        --
        -- These are defined as:
        -- E + P = T and P = E * f
        --
        -- Rewritten as:
        -- E = T - P
        -- E = P / f
        --
        -- P / f = T - P
        --
        -- We calculate:
        --   * P based on E and f
        --   * f based on E and P
        -- and compare that to the actual P and f.
        --
        -- We then suggest "fixed" versions of all three of the parts by computing
        -- them from the other two:
        --
        -- Each defined in terms of the other two:
        -- P = E * f
        -- E = P / f
        -- f = P / E
        case Account.fraction rounding previousAccount ratio of
          (Nothing, _) -> validationFailure $ BalanceErrorAmountRatioFraction' tl pal ratio
          (Just computedP, _) ->
            when (computedP /= thisAccount) $ do
              mComputedE <-
                if ratio == 0
                  then pure Nothing
                  else do
                    let factor = 1 / ratio
                    case fst $ Account.fraction rounding thisAccount factor of
                      Nothing -> validationFailure $ BalanceErrorAmountRatioFraction' tl al factor
                      Just cE -> pure $ Just $ Located pal cE
              let mComputedRatio = do
                    let Located _ qf = currencyQuantisationFactor currency
                    rate <- ConversionRate.toRational <$> Account.rate qf previousAccount qf thisAccount
                    pure $ Located rl rate

              validationFailure $
                BalanceErrorAmountRatio
                  -- Transaction location
                  tl
                  -- Posting location
                  pl
                  -- Previous posting location
                  ppl
                  -- AmountRatio location
                  pctl
                  -- Currency
                  currency
                  -- Computed P
                  (Located al computedP)
                  -- Computed E
                  mComputedE
                  -- Computed f
                  mComputedRatio

filterAccountBalances :: Filter.Filter -> AccountBalances ann -> AccountBalances ann
filterAccountBalances f =
  M.filterWithKey (\an _ -> Filter.predicate f an)

convertAccountBalances ::
  forall ann.
  (Ord ann) =>
  MemoisedPriceGraph (Currency ann) ->
  Currency ann ->
  AccountBalances ann ->
  Validation (ConvertError ann) (AccountBalances ann)
convertAccountBalances mpg currencyTo =
  traverse (convertMultiAccount Nothing mpg currencyTo)

fillAccountBalances ::
  forall ann.
  (Ord ann) =>
  AccountBalances ann ->
  Validation (BalanceError ann) (AccountBalances ann)
fillAccountBalances bs = foldM go bs (M.toList bs)
  where
    go ::
      AccountBalances ann ->
      (AccountName, Money.MultiAccount (Currency ann)) ->
      Validation (BalanceError ann) (AccountBalances ann)
    go as (an, am) = foldM (go' am) as (AccountName.ancestors an)
    go' ::
      Money.MultiAccount (Currency ann) ->
      AccountBalances ann ->
      AccountName ->
      Validation (BalanceError ann) (AccountBalances ann)
    go' am as an = case M.lookup an as of
      Nothing -> pure $ M.insert an am as
      Just am' -> case MultiAccount.add am am' of
        Nothing -> validationFailure BalanceErrorCouldNotFill
        Just am'' -> pure $ M.insert an am'' as

-- | Produce an evaluated ledger from a compiled ledger.
--
-- Transactions are processed in file order (matching the compiled ledger),
-- with price declarations interleaved before the first transaction whose day
-- is >= the price's day. This ensures:
-- 1. Account balances accumulate in file order (matching assertion behavior)
-- 2. Price graphs are cumulative by day (a same-day price is available for
--    same-day transactions)
produceEvaluatedLedger ::
  forall ann.
  (Ord ann) =>
  Ledger ann ->
  Validation (EvaluatedLedgerError ann) (EvaluatedLedger ann)
produceEvaluatedLedger ledger = do
  let transactions = ledgerTransactions ledger
  let prices = ledgerPrices ledger

  -- Validate all transactions (get both assertion balances and with-virtual balances)
  validatedTransactions <-
    mapValidationFailure EvaluatedLedgerErrorBalanceError $
      V.mapM (\t -> (,) t <$> balanceTransaction True t) transactions

  -- Interleave prices into the transaction stream.
  -- Transactions are kept in file order; prices are inserted before the first
  -- transaction whose day is >= the price's day.
  let mergedEntries = mergeEntries validatedTransactions prices

  -- Process entries to build cumulative state
  processEntries ledger mergedEntries

-- | Check account type assertions and explicit assertions on an evaluated ledger.
--
-- This is separated from 'produceEvaluatedLedger' so that reports which don't
-- need assertion checking (e.g. register) can produce the evaluated ledger
-- without failing on assertion violations.
checkEvaluatedLedgerAssertions ::
  (Ord ann) =>
  EvaluatedLedger ann ->
  Validation (EvaluatedLedgerError ann) ()
checkEvaluatedLedgerAssertions evaluatedLedger =
  let ledger = evaluatedLedgerSource evaluatedLedger
      accounts = ledgerAccounts ledger
   in V.mapM_ (checkEntryAssertions accounts) (evaluatedLedgerEntries evaluatedLedger)

checkEntryAssertions ::
  (Ord ann) =>
  Map AccountName (GenLocated ann (Account ann)) ->
  EvaluatedEntry ann ->
  Validation (EvaluatedLedgerError ann) ()
checkEntryAssertions _ (EvaluatedEntryPrice _) = pure ()
checkEntryAssertions accounts (EvaluatedEntryTransaction evaluatedTransaction) = do
  let Located tl t = evaluatedTransactionLocated evaluatedTransaction
      balancesWithoutVirtual = evaluatedTransactionBalancesWithoutVirtual evaluatedTransaction
  checkAccountTypeAssertions accounts tl balancesWithoutVirtual
  traverse_ (checkAssertion tl balancesWithoutVirtual) (transactionAssertions t)

-- | An intermediate merged entry before evaluation
data MergedEntry ann
  = MergedTransaction
      !(GenLocated ann (Transaction ann))
      -- | (forAssertions/withoutVirtual, forUserWithVirtual)
      !(GenLocated ann (AccountBalances ann), GenLocated ann (AccountBalances ann))
  | MergedPrice !(GenLocated ann (Price ann))

-- | Interleave prices into the transaction stream.
--
-- Transactions are kept in their original (file) order. Prices are sorted
-- by day and inserted before the first transaction whose day is >= the
-- price's day. This means a price declared on a given day is available
-- for all transactions on that day, regardless of file order.
mergeEntries ::
  forall ann.
  Vector
    ( GenLocated ann (Transaction ann),
      (GenLocated ann (AccountBalances ann), GenLocated ann (AccountBalances ann))
    ) ->
  Vector (GenLocated ann (Price ann)) ->
  [MergedEntry ann]
mergeEntries transactions prices =
  go (V.toList transactions) (V.toList prices)
  where
    go ::
      [ ( GenLocated ann (Transaction ann),
          (GenLocated ann (AccountBalances ann), GenLocated ann (AccountBalances ann))
        )
      ] ->
      [GenLocated ann (Price ann)] ->
      [MergedEntry ann]
    go [] remainingPrices = map MergedPrice remainingPrices
    go ((lt@(Located _ t), bals) : ts) remainingPrices =
      let tDay = transactionDay t
          -- Take all prices whose day <= this transaction's day
          (pricesBefore, pricesAfter) = span (\lp -> priceDay lp <= tDay) remainingPrices
       in map MergedPrice pricesBefore
            ++ [MergedTransaction lt bals]
            ++ go ts pricesAfter

    transactionDay :: Transaction ann -> Day
    transactionDay t =
      let Located _ ts = transactionTimestamp t
       in Timestamp.toDay ts

    priceDay :: GenLocated ann (Price ann) -> Day
    priceDay (Located _ p) =
      let Located _ ts = priceTimestamp p
       in Timestamp.toDay ts

-- | Running state during entry processing
data ProcessState ann = ProcessState
  { processStateBalancesWithVirtual :: !(AccountBalances ann),
    processStateBalancesWithoutVirtual :: !(AccountBalances ann),
    processStatePriceGraph :: !(PriceGraph Day (Currency ann)),
    processStateMemoisedPriceGraph :: !(MemoisedPriceGraph (Currency ann))
  }

processEntries ::
  forall ann.
  (Ord ann) =>
  Ledger ann ->
  [MergedEntry ann] ->
  Validation (EvaluatedLedgerError ann) (EvaluatedLedger ann)
processEntries ledger mergedEntries = do
  let initialState =
        ProcessState
          { processStateBalancesWithVirtual = M.empty,
            processStateBalancesWithoutVirtual = M.empty,
            processStatePriceGraph = PriceGraph.empty,
            processStateMemoisedPriceGraph = MemoisedPriceGraph.empty
          }

  let go ::
        ProcessState ann ->
        [MergedEntry ann] ->
        Validation (EvaluatedLedgerError ann) [EvaluatedEntry ann]
      go _ [] = pure []
      go state (entry : rest) = do
        (evaluatedEntry, newState) <- processEntry state entry
        restEntries <- go newState rest
        pure (evaluatedEntry : restEntries)

  entries <- go initialState mergedEntries
  pure
    EvaluatedLedger
      { evaluatedLedgerSource = ledger,
        evaluatedLedgerEntries = V.fromList entries
      }

processEntry ::
  forall ann.
  (Ord ann) =>
  ProcessState ann ->
  MergedEntry ann ->
  Validation (EvaluatedLedgerError ann) (EvaluatedEntry ann, ProcessState ann)
processEntry state (MergedPrice lp@(Located _ Price {..})) =
  let Located _ currencyFrom = priceCurrency
      Located _ Cost {..} = priceCost
      Located _ rate = costConversionRate
      Located _ currencyTo = costCurrency
      Located _ timestamp = priceTimestamp
      priority = Timestamp.toDay timestamp
      newPriceGraph = PriceGraph.insert currencyFrom currencyTo rate priority (processStatePriceGraph state)
      newMemoisedPriceGraph = MemoisedPriceGraph.fromPriceGraph newPriceGraph
      newState =
        state
          { processStatePriceGraph = newPriceGraph,
            processStateMemoisedPriceGraph = newMemoisedPriceGraph
          }
      evaluatedPrice =
        EvaluatedPrice
          { evaluatedPriceLocated = lp,
            evaluatedPriceBalancesWithVirtual = processStateBalancesWithVirtual state,
            evaluatedPriceBalancesWithoutVirtual = processStateBalancesWithoutVirtual state,
            evaluatedPricePriceGraph = newMemoisedPriceGraph
          }
   in pure (EvaluatedEntryPrice evaluatedPrice, newState)
processEntry state (MergedTransaction lt@(Located tl t) (Located _ sumForAssertions, Located _ sumForVirtual)) = do
  -- Incorporate assertion balances (without virtual)
  newBalancesWithoutVirtual <- incorporateAccounts tl (processStateBalancesWithoutVirtual state) sumForAssertions

  -- Incorporate user-visible balances (with virtual postings)
  newBalancesWithVirtual <- incorporateAccounts tl (processStateBalancesWithVirtual state) sumForVirtual

  -- Build per-posting evaluated postings
  let currentPriceGraph = processStateMemoisedPriceGraph state
  evaluatedPostings <- buildEvaluatedPostings tl (processStateBalancesWithVirtual state) (processStateBalancesWithoutVirtual state) currentPriceGraph (transactionPostings t)

  let evaluatedTransaction =
        EvaluatedTransaction
          { evaluatedTransactionLocated = lt,
            evaluatedTransactionPostings = evaluatedPostings,
            evaluatedTransactionBalancesWithVirtual = newBalancesWithVirtual,
            evaluatedTransactionBalancesWithoutVirtual = newBalancesWithoutVirtual,
            evaluatedTransactionPriceGraph = currentPriceGraph
          }
      newState =
        state
          { processStateBalancesWithVirtual = newBalancesWithVirtual,
            processStateBalancesWithoutVirtual = newBalancesWithoutVirtual
          }
  pure (EvaluatedEntryTransaction evaluatedTransaction, newState)

incorporateAccounts ::
  (Ord ann) =>
  ann ->
  AccountBalances ann ->
  AccountBalances ann ->
  Validation (EvaluatedLedgerError ann) (AccountBalances ann)
incorporateAccounts l totals currents =
  foldlM (incorporateAccount l) totals (M.toList currents)

incorporateAccount ::
  (Ord ann) =>
  ann ->
  AccountBalances ann ->
  (AccountName, Money.MultiAccount (Currency ann)) ->
  Validation (EvaluatedLedgerError ann) (AccountBalances ann)
incorporateAccount l totals (an, current) = case M.lookup an totals of
  Nothing -> pure $ M.insert an current totals
  Just total -> case MultiAccount.add total current of
    Nothing -> validationFailure $ EvaluatedLedgerErrorCouldNotAddTransaction l an total current
    Just new -> pure $ M.insert an new totals

checkAccountTypeAssertions ::
  Map AccountName (GenLocated ann (Account ann)) ->
  ann ->
  AccountBalances ann ->
  Validation (EvaluatedLedgerError ann) ()
checkAccountTypeAssertions accounts tl =
  traverse_
    ( \(an, ab) -> case M.lookup an accounts of
        Nothing -> validationFailure $ EvaluatedLedgerErrorUndeclaredAccount tl an
        Just (Located adl acc) -> do
          let at = accountType acc
              predicate = AccountType.assertion at
          if all predicate (MultiAccount.unMultiAccount ab)
            then pure ()
            else validationFailure $ EvaluatedLedgerErrorAccountTypeAssertion tl adl at ab
    )
    . M.toList

checkAssertion ::
  (Ord ann) =>
  ann ->
  AccountBalances ann ->
  GenLocated ann (Assertion ann) ->
  Validation (EvaluatedLedgerError ann) ()
checkAssertion tl runningTotal a@(Located _ (AssertionEquals lan la lcs)) = do
  let Located _ an = lan
  let Located _ expected = la
  let Located _ c = lcs
  let actualMulti = fromMaybe MultiAccount.zero $ M.lookup an runningTotal
  let actual = fromMaybe Account.zero $ M.lookup c $ MultiAccount.unMultiAccount actualMulti
  if actual == expected
    then pure ()
    else validationFailure $ EvaluatedLedgerErrorAssertion tl a actualMulti (Account.subtract actual expected)

-- | Strict left fold over a list in a monadic context
foldlM ::
  (Monad m) =>
  (b -> a -> m b) ->
  b ->
  [a] ->
  m b
foldlM _ z [] = pure z
foldlM f z (x : xs) = f z x >>= \z' -> foldlM f z' xs

buildEvaluatedPostings ::
  forall ann.
  (Ord ann) =>
  ann ->
  AccountBalances ann ->
  AccountBalances ann ->
  MemoisedPriceGraph (Currency ann) ->
  Vector (GenLocated ann (Posting ann)) ->
  Validation (EvaluatedLedgerError ann) (Vector (EvaluatedPosting ann))
buildEvaluatedPostings tl initialWithVirtual initialWithoutVirtual priceGraph postings =
  let go ::
        (Int, AccountBalances ann, AccountBalances ann) ->
        Validation
          (EvaluatedLedgerError ann)
          ( EvaluatedPosting ann,
            (Int, AccountBalances ann, AccountBalances ann)
          )
      go (ix, balWithVirtual, balWithoutVirtual) =
        let lp@(Located pl Posting {..}) = V.unsafeIndex postings ix
            Located _ currency = postingCurrency
            Located _ account = postingAccount
            Located _ an = postingAccountName
            amount = MultiAccount.fromAccount currency account
         in do
              -- Update without-virtual balances (real postings only)
              newBalWithoutVirtual <-
                if postingReal
                  then addToBalances tl an pl currency account balWithoutVirtual
                  else pure balWithoutVirtual

              -- Update with-virtual balances (all postings)
              newBalWithVirtual <- addToBalances tl an pl currency account balWithVirtual

              let evaluatedPosting =
                    EvaluatedPosting
                      { evaluatedPostingLocated = lp,
                        evaluatedPostingAmount = amount,
                        evaluatedPostingBalancesWithVirtual = newBalWithVirtual,
                        evaluatedPostingBalancesWithoutVirtual = newBalWithoutVirtual,
                        evaluatedPostingPriceGraph = priceGraph
                      }
              pure (evaluatedPosting, (succ ix, newBalWithVirtual, newBalWithoutVirtual))
   in V.unfoldrExactNM (V.length postings) go (0, initialWithVirtual, initialWithoutVirtual)

addToBalances ::
  (Ord ann) =>
  ann ->
  AccountName ->
  ann ->
  Currency ann ->
  Money.Account ->
  AccountBalances ann ->
  Validation (EvaluatedLedgerError ann) (AccountBalances ann)
addToBalances tl an pl c a bs = case M.lookup an bs of
  Nothing -> pure $ M.insert an (MultiAccount.fromAccount c a) bs
  Just a' ->
    case MultiAccount.addAccount a' c a of
      Nothing -> validationFailure $ EvaluatedLedgerErrorCouldNotAddPostings tl an pl a' c a
      Just a'' -> pure $ M.insert an a'' bs

-- Helpers for error formatting

multiAccountLines :: Money.MultiAccount (Currency ann) -> [String]
multiAccountLines = map (uncurry accountLine) . M.toList . MultiAccount.unMultiAccount

accountLine :: Currency ann -> Money.Account -> String
accountLine c a =
  let Located _ qf = currencyQuantisationFactor c
   in unwords
        [ Account.format qf a,
          T.unpack $ currencySymbolText $ currencySymbol c
        ]

unlines' :: [String] -> String
unlines' = intercalate "\n"
