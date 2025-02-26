{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Balance
  ( BalanceReport (..),
    AccountBalances,
    BalanceError (..),
    produceBalanceReport,
    balanceTransaction,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.AccountType as AccountType
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import qualified Centjes.Filter as Filter
import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Data.Traversable
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
import Numeric.Natural

newtype BalancedLedger ann = BalancedLedger {balancedLedgerTransactions :: Vector (GenLocated ann (Transaction ann), AccountBalances ann)}

type AccountBalances ann = Map AccountName (Money.MultiAccount (Currency ann))

data BalanceReport ann = BalanceReport
  { balanceReportBalances :: !(AccountBalances ann),
    balanceReportFilledBalances :: !(AccountBalances ann),
    balanceReportTotal :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceReport ann) where
  validate br@BalanceReport {..} =
    mconcat
      [ genericValidate br,
        declare "The total matches the balances" $
          MultiAccount.sum balanceReportBalances == Just balanceReportTotal
      ]

data BalanceError ann
  = BalanceErrorCouldNotAddTransaction !ann !AccountName !(Money.MultiAccount (Currency ann)) !(Money.MultiAccount (Currency ann))
  | BalanceErrorCouldNotAddPostings !ann !AccountName !ann !(Money.MultiAccount (Currency ann)) !(Currency ann) !Money.Account
  | BalanceErrorCouldNotSumPostings !ann ![Money.MultiAccount (Currency ann)]
  | BalanceErrorConversionTooBig !(GenLocated ann Money.Account) !(GenLocated ann (Cost ann))
  | BalanceErrorConversionImpossibleRate !(GenLocated ann Money.Account) !(GenLocated ann (Cost ann)) !(Maybe Money.ConversionRate)
  | BalanceErrorUndeclaredAccount !ann !AccountName
  | BalanceErrorTransactionOffBalance !ann !(Money.MultiAccount (Currency ann)) ![GenLocated ann (Posting ann)]
  | BalanceErrorPercentageNoPrevious !ann !ann
  | BalanceErrorPercentageCurrency !ann !ann !ann
  | BalanceErrorPercentageFraction !ann !ann !ann !Bool
  | BalanceErrorPercentageFraction' !ann !ann !(Ratio Natural)
  | BalanceErrorPercentage !ann !ann !ann !ann !(Currency ann) !(GenLocated ann Money.Account) !(Maybe (GenLocated ann Money.Account)) !(Maybe (GenLocated ann (Ratio Natural)))
  | BalanceErrorAccountTypeAssertion !ann !ann !AccountType !(Money.MultiAccount (Currency ann))
  | BalanceErrorAssertion !ann !(GenLocated ann (Assertion ann)) !(Money.MultiAccount (Currency ann)) !(Maybe Money.Account)
  | BalanceErrorConvertError !(ConvertError ann)
  | BalanceErrorCouldNotFill
  | BalanceErrorCouldNotSumTotal ![Money.MultiAccount (Currency ann)]
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceError ann)

instance ToReport (BalanceError SourceSpan) where
  toReport = \case
    BalanceErrorCouldNotAddTransaction s an subtotal current ->
      Err
        (Just "BE_RUNNING_BALANCE")
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
    BalanceErrorCouldNotSumPostings s mas ->
      Err
        (Just "BE_TRANSACTION_SUM")
        "Could not sum postings within transaction"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition s, This "Could not sum these amounts together because the result would become too big:"),
          (toDiagnosePosition s, Where $ unlines' $ concatMap multiAccountLines mas)
        ]
        []
    BalanceErrorConversionTooBig (Located al _) (Located cl _) ->
      Err
        (Just "BE_CONVERSION_TOO_BIG")
        "Could not convert amount because it got too big"
        [ (toDiagnosePosition cl, Where "Using this cost"),
          (toDiagnosePosition al, This "Could not convert this amount")
        ]
        []
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
    BalanceErrorUndeclaredAccount s an ->
      Err
        (Just "BE_UNDECLARED_ACCOUNT")
        (unwords ["This account has not been declared:", AccountName.toString an])
        [(toDiagnosePosition s, Where "While trying to balance this transaction")]
        [Note "This is an internal exception and should never trigger past compilation"]
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
    BalanceErrorPercentageNoPrevious s pctl ->
      Err
        (Just "BE_PERCENTAGE_NO_PREVIOUS")
        "Posting with a percentage has no previous posting"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition pctl, This "This percentage requires a previous posting")
        ]
        []
    BalanceErrorPercentageCurrency s pcl cl ->
      Err
        (Just "BE_PERCENTAGE_CURRENCY")
        "Posting with percentage has different currency than the previous posting"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition pcl, This "This currency is different ..."),
          (toDiagnosePosition cl, This "... from this currency")
        ]
        []
    BalanceErrorPercentageFraction s al rl isInverse ->
      Err
        (Just "BE_PERCENTAGE_FRACTION")
        ""
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition al, This $ unwords ["Could not", if isInverse then "divide" else "multiply", "this amount"]),
          (toDiagnosePosition rl, This "... by this percentage because the amount would get too big.")
        ]
        []
    BalanceErrorPercentageFraction' s al ratio ->
      Err
        (Just "BE_PERCENTAGE_FRACTION")
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
    BalanceErrorPercentage s pl ppl pctl currency (Located pal computedPrevious) mComputedCurrent mComputedRatio ->
      let Located _ qf = currencyQuantisationFactor currency
       in Err
            (Just "BE_PERCENTAGE")
            "The given percentage does not match the amount it describes."
            ( concat
                [ [ (toDiagnosePosition pctl, This "Using this percentage")
                  ],
                  [ ( toDiagnosePosition rl,
                      Maybe $
                        unlines
                          [ "Perhaps this percentage needs to be",
                            DecimalLiteral.toString dl <> "%"
                          ]
                    )
                    | Located rl expectedRatio <- maybeToList mComputedRatio,
                      dl <- maybeToList $ DecimalLiteral.fromRatio (expectedRatio * 100)
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
                    (toDiagnosePosition pl, Where "While checking the percentage in this posting"),
                    (toDiagnosePosition ppl, Where "Which relates to the amount in this posting")
                  ]
                ]
            )
            []
    BalanceErrorAccountTypeAssertion s adl at bal ->
      Err
        (Just "BE_ACCOUNT_TYPE_ASSERTION")
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
    BalanceErrorAssertion s (Located al (AssertionEquals _ (Located _ asserted) (Located _ c))) actual mDifference ->
      Err
        (Just "BE_ASSERTION")
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
    BalanceErrorConvertError ce -> toReport ce
    BalanceErrorCouldNotFill ->
      Err
        (Just "BE_FILL")
        "Could not fill accounts hierarchically because the result got too big."
        []
        []
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

multiAccountLines :: Money.MultiAccount (Currency ann) -> [String]
multiAccountLines = map (uncurry accountLine) . M.toList . MultiAccount.unMultiAccount

accountLine :: Currency ann -> Money.Account -> String
accountLine c a =
  let Located _ qf = currencyQuantisationFactor c
   in unwords
        [ Account.format qf a,
          T.unpack $ currencySymbolText $ currencySymbol c
        ]

produceBalanceReport ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Maybe Year ->
  Maybe CurrencySymbol ->
  Bool ->
  Ledger ann ->
  Validation (BalanceError ann) (BalanceReport ann)
produceBalanceReport f mYear mCurrencySymbolTo showVirtual l = do
  let filterByYear = maybe pure filterBalancedLedgerByYear mYear
  bl <- produceBalancedLedger showVirtual l >>= filterByYear
  let v = balancedLedgerTransactions bl
  let balances =
        filterAccountBalances f $
          if V.null v
            then M.empty
            else snd (V.last v)

  balanceReportBalances <- mapValidationFailure BalanceErrorConvertError $ case mCurrencySymbolTo of
    Nothing -> pure balances
    Just currencySymbolTo -> do
      currencyTo <- lookupConversionCurrency (ledgerCurrencies l) currencySymbolTo
      convertAccountBalances
        (pricesToPriceGraph (ledgerPrices l))
        currencyTo
        balances

  balanceReportFilledBalances <- fillAccountBalances balanceReportBalances

  balanceReportTotal <- case MultiAccount.sum balanceReportBalances of
    Nothing -> validationFailure $ BalanceErrorCouldNotSumTotal $ M.elems balanceReportBalances
    Just total -> pure total

  pure BalanceReport {..}

-- A balanced ledger has all account balances right after any transaction ready.
-- We have to:
--
-- 1. throw away transactions until we find one that is included,
-- 2. subtract the latest thrown-away balances from all the leftover balances
-- 3. throw away the rest of the balances after the given year.
filterBalancedLedgerByYear :: (Ord ann) => Year -> BalancedLedger ann -> Validation (BalanceError ann) (BalancedLedger ann)
filterBalancedLedgerByYear y (BalancedLedger v) = BalancedLedger . V.fromList <$> goBefore M.empty (V.toList v)
  where
    goBefore ::
      (Ord ann) =>
      AccountBalances ann ->
      [(GenLocated ann (Transaction ann), AccountBalances ann)] ->
      Validation (BalanceError ann) [(GenLocated ann (Transaction ann), AccountBalances ann)]
    goBefore toSubtract = \case
      [] -> pure []
      ts@((lTrans, balances) : rest) ->
        if timestampMatchesYear lTrans
          then goAfter toSubtract ts
          else goBefore balances rest

    goAfter ::
      (Ord ann) =>
      AccountBalances ann ->
      [(GenLocated ann (Transaction ann), AccountBalances ann)] ->
      Validation (BalanceError ann) [(GenLocated ann (Transaction ann), AccountBalances ann)]
    goAfter toSubtract tups = mapM goSubtract $ takeWhile (timestampMatchesYear . fst) tups
      where
        goSubtract (lt, balances) = (,) lt <$> subtractAccountBalances balances toSubtract

    timestampMatchesYear (Located _ Transaction {..}) =
      let Located _ ts = transactionTimestamp
          d = Timestamp.toDay ts
          (y', _, _) = toGregorian d
       in y' == y

subtractAccountBalances ::
  (Ord ann) =>
  AccountBalances ann ->
  AccountBalances ann ->
  Validation (BalanceError ann) (AccountBalances ann)
subtractAccountBalances total toSubtract = sequence $ M.unionWith go (M.map pure total) (M.map pure toSubtract)
  where
    go mTot mSub = do
      tot <- mTot
      sub <- mSub
      case MultiAccount.subtract tot sub of
        Nothing -> undefined
        Just res -> pure res

filterAccountBalances :: Filter -> AccountBalances ann -> AccountBalances ann
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

produceBalancedLedger ::
  forall ann.
  (Ord ann) =>
  Bool ->
  Ledger ann ->
  Validation (BalanceError ann) (BalancedLedger ann)
produceBalancedLedger showVirtual ledger = do
  tups <- for (ledgerTransactions ledger) $ \t ->
    (,) t <$> balanceTransaction showVirtual t

  let constructBalancedVector ::
        (Int, AccountBalances ann, AccountBalances ann) ->
        Validation
          (BalanceError ann)
          ( (GenLocated ann (Transaction ann), AccountBalances ann),
            (Int, AccountBalances ann, AccountBalances ann)
          )
      constructBalancedVector (ix, runningTotalForAssertions, runningTotalForResult) = do
        let (lt@(Located tl t), (sumForAssertions, sumForResult)) = V.unsafeIndex tups ix
        newTotalForAssertions <- incorporateAccounts runningTotalForAssertions sumForAssertions
        newTotalForResult <- incorporateAccounts runningTotalForResult sumForResult

        checkAccountTypeAssertions (ledgerAccounts ledger) tl newTotalForAssertions
        traverse_ (checkAssertion tl newTotalForAssertions) (transactionAssertions t)

        pure ((lt, newTotalForResult), (succ ix, newTotalForAssertions, newTotalForResult))

  BalancedLedger <$> V.unfoldrExactNM (V.length tups) constructBalancedVector (0, M.empty, M.empty)
  where
    incorporateAccounts ::
      AccountBalances ann ->
      GenLocated ann (AccountBalances ann) ->
      Validation
        (BalanceError ann)
        (AccountBalances ann)
    incorporateAccounts
      totals
      (Located l currents) = foldM (incorporateAccount l) totals (M.toList currents)

    incorporateAccount ::
      ann ->
      AccountBalances ann ->
      (AccountName, Money.MultiAccount (Currency ann)) ->
      Validation (BalanceError ann) (AccountBalances ann)
    incorporateAccount l totals (an, current) = case M.lookup an totals of
      Nothing -> pure $ M.insert an current totals
      Just total -> case MultiAccount.add total current of
        Nothing -> validationFailure $ BalanceErrorCouldNotAddTransaction l an total current
        Just new -> pure $ M.insert an new totals

checkAccountTypeAssertions ::
  Map AccountName (GenLocated ann (Account ann)) ->
  ann ->
  AccountBalances ann ->
  Validation (BalanceError ann) ()
checkAccountTypeAssertions accounts tl =
  traverse_
    ( \(an, ab) -> case M.lookup an accounts of
        Nothing -> validationFailure $ BalanceErrorUndeclaredAccount tl an
        Just (Located adl acc) -> do
          let at = accountType acc
              predicate = AccountType.assertion at
          if all predicate (MultiAccount.unMultiAccount ab)
            then pure ()
            else validationFailure $ BalanceErrorAccountTypeAssertion tl adl at ab
    )
    . M.toList

checkAssertion ::
  (Ord ann) =>
  ann ->
  AccountBalances ann ->
  GenLocated ann (Assertion ann) ->
  Validation (BalanceError ann) ()
checkAssertion tl runningTotal a@(Located _ (AssertionEquals lan la lcs)) = do
  let Located _ an = lan
  let Located _ expected = la
  let Located _ c = lcs
  let actualMulti = fromMaybe MultiAccount.zero $ M.lookup an runningTotal
  let actual = fromMaybe Account.zero $ M.lookup c $ MultiAccount.unMultiAccount actualMulti
  if actual == expected
    then pure ()
    else validationFailure $ BalanceErrorAssertion tl a actualMulti (Account.subtract actual expected)

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
        (Located pl (Posting real (Located _ an) lc@(Located _ currency) la@(Located _ account) mCost mPercentage)) = do
          for_ mPercentage $ \lpct@(Located pctl _) -> do
            let go i =
                  case transactionPostings V.!? pred i of
                    Nothing -> validationFailure $ BalanceErrorPercentageNoPrevious tl pctl
                    Just lp@(Located _ p) ->
                      if postingReal p
                        then pure lp
                        else go (pred i)

            lp <- go ix
            checkPercentage tl pl lp lc la lpct

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
              else pure balancesForAssertions

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

checkPercentage ::
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
  -- Current posting's percentage
  GenLocated ann (Percentage ann) ->
  Validation (BalanceError ann) ()
checkPercentage
  tl
  pl
  (Located ppl p)
  (Located cl currency)
  (Located al thisAccount)
  (Located pctl (Percentage inclusive rounding (Located rl ratio))) = do
    let Located pcl newCurrency = postingCurrency p
    when (newCurrency /= currency) $ validationFailure $ BalanceErrorPercentageCurrency tl pcl cl
    let Located pal previousAccount = postingAccount p
    if inclusive
      then do
        -- T: Total amount (previous posting's amount)
        -- P: Part amount (this posting's amount)
        -- f: Fraction (The percentage)
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
        (computedP, whereToRoundNext) <- case Account.fractionRatio rounding previousAccount computedPRatio of
          (Nothing, _) -> validationFailure $ BalanceErrorPercentageFraction' tl pal computedPRatio
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
                case fst $ Account.fractionRatio whereToRoundNext thisAccount computedTRatio of
                  Nothing -> validationFailure $ BalanceErrorPercentageFraction' tl al computedTRatio
                  Just cT -> pure $ Just $ Located pal cT

          let mComputedRatio = do
                let Located _ qf = currencyQuantisationFactor currency
                rate <- ConversionRate.toRatio <$> Account.rate qf previousAccount qf thisAccount
                pure $ Located rl $ rate * (1 + ratio)

          validationFailure $
            BalanceErrorPercentage
              -- Transaction location
              tl
              -- Posting location
              pl
              -- Previous posting location
              ppl
              -- Percentage location
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
        -- f: Fraction (The percentage)
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
        case Account.fractionRatio rounding previousAccount ratio of
          (Nothing, _) -> validationFailure $ BalanceErrorPercentageFraction' tl pal ratio
          (Just computedP, _) ->
            when (computedP /= thisAccount) $ do
              mComputedE <-
                if ratio == 0
                  then pure Nothing
                  else do
                    let factor = 1 / ratio
                    case fst $ Account.fractionRatio rounding thisAccount factor of
                      Nothing -> validationFailure $ BalanceErrorPercentageFraction' tl al factor
                      Just cE -> pure $ Just $ Located pal cE
              let mComputedRatio = do
                    let Located _ qf = currencyQuantisationFactor currency
                    rate <- ConversionRate.toRatio <$> Account.rate qf previousAccount qf thisAccount
                    pure $ Located rl rate

              validationFailure $
                BalanceErrorPercentage
                  -- Transaction location
                  tl
                  -- Posting location
                  pl
                  -- Previous posting location
                  ppl
                  -- Percentage location
                  pctl
                  -- Currency
                  currency
                  -- Computed P
                  (Located al computedP)
                  -- Computed E
                  mComputedE
                  -- Computed f
                  mComputedRatio

unlines' :: [String] -> String
unlines' = intercalate "\n"
