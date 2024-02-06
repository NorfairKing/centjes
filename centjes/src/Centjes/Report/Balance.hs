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
import Centjes.Validation
import Control.DeepSeq
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
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalancedLedger ann)

instance NFData ann => NFData (BalancedLedger ann)

type AccountBalances ann = Map AccountName (Money.MultiAccount (Currency ann))

data BalanceReport ann = BalanceReport
  { balanceReportBalances :: !(AccountBalances ann),
    balanceReportFilledBalances :: !(AccountBalances ann),
    balanceReportTotal :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceReport ann) where
  validate br@BalanceReport {..} =
    mconcat
      [ genericValidate br,
        declare "The total matches the balances" $
          MultiAccount.sum balanceReportBalances == Just balanceReportTotal
      ]

instance NFData ann => NFData (BalanceReport ann)

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
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceError ann)

instance NFData ann => NFData (BalanceError ann)

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
        [ Hint $ "The closest valid rate is " <> DecimalLiteral.format dl
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
                            DecimalLiteral.format dl <> "%"
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
                    DecimalLiteral.format suggestedLiteral,
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
  Ord ann =>
  Filter ->
  Maybe CurrencySymbol ->
  Ledger ann ->
  Validation (BalanceError ann) (BalanceReport ann)
produceBalanceReport f mCurrencySymbolTo l = do
  bl <- produceBalancedLedger l
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

filterAccountBalances :: Filter -> AccountBalances ann -> AccountBalances ann
filterAccountBalances f =
  M.filterWithKey (\an _ -> Filter.predicate f an)

convertAccountBalances ::
  forall ann.
  Ord ann =>
  MemoisedPriceGraph (Currency ann) ->
  Currency ann ->
  AccountBalances ann ->
  Validation (ConvertError ann) (AccountBalances ann)
convertAccountBalances mpg currencyTo =
  traverse (convertMultiAccount mpg currencyTo)

fillAccountBalances ::
  forall ann.
  Ord ann =>
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
  Ord ann =>
  Ledger ann ->
  Validation (BalanceError ann) (BalancedLedger ann)
produceBalancedLedger ledger = do
  tups <- for (ledgerTransactions ledger) $ \t ->
    (,) t <$> balanceTransaction t

  let constructBalancedVector ::
        (Int, AccountBalances ann) ->
        Validation
          (BalanceError ann)
          ( (GenLocated ann (Transaction ann), AccountBalances ann),
            (Int, AccountBalances ann)
          )
      constructBalancedVector (ix, runningTotal) = do
        let (lt@(Located tl t), ab) = V.unsafeIndex tups ix
        newTotal <- incorporateAccounts runningTotal ab

        checkAccountTypeAssertions (ledgerAccounts ledger) tl newTotal
        traverse_ (checkAssertion tl newTotal) (transactionAssertions t)

        pure ((lt, newTotal), (succ ix, newTotal))

  BalancedLedger <$> V.unfoldrExactNM (V.length tups) constructBalancedVector (0, M.empty)
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
  Map AccountName (GenLocated ann AccountType) ->
  ann ->
  AccountBalances ann ->
  Validation (BalanceError ann) ()
checkAccountTypeAssertions accounts tl =
  traverse_
    ( \(an, ab) -> case M.lookup an accounts of
        Nothing -> validationFailure $ BalanceErrorUndeclaredAccount tl an
        Just (Located adl at) -> do
          let predicate = AccountType.assertion at
          if all predicate (MultiAccount.unMultiAccount ab)
            then pure ()
            else validationFailure $ BalanceErrorAccountTypeAssertion tl adl at ab
    )
    . M.toList

checkAssertion ::
  Ord ann =>
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
  Ord ann =>
  GenLocated ann (Transaction ann) ->
  Validation (BalanceError ann) (GenLocated ann (AccountBalances ann))
balanceTransaction (Located tl Transaction {..}) = do
  let incorporatePosting ::
        -- (Balances for transaction balance checking, balances of acounts)
        (AccountBalances ann, AccountBalances ann) ->
        Int ->
        GenLocated ann (Posting ann) ->
        Validation (BalanceError ann) (AccountBalances ann, AccountBalances ann)
      incorporatePosting
        (convertedBalances, actualBalances)
        ix
        (Located pl (Posting real (Located _ an) lc@(Located _ currency) la@(Located _ account) mCost mPercentage)) = do
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

          let addAccountToBalances ::
                Currency ann ->
                Money.Account ->
                AccountBalances ann ->
                Validation (BalanceError ann) (AccountBalances ann)
              addAccountToBalances c a bs = case M.lookup an bs of
                Nothing -> pure $ M.insert an (MultiAccount.fromAccount c a) bs
                Just a' ->
                  case MultiAccount.addAccount a' c a of
                    Nothing -> validationFailure $ BalanceErrorCouldNotAddPostings tl an pl a' c a
                    Just a'' -> pure $ M.insert an a'' bs

          forM_ mPercentage $ \lpct@(Located pctl _) -> do
            lp <-
              case transactionPostings V.!? pred ix of
                Nothing -> validationFailure $ BalanceErrorPercentageNoPrevious tl pctl
                Just lp -> pure lp
            checkPercentage tl pl lp lc la lpct

          convertedBalances' <-
            if real -- Don't count virtual postings for balancing
              then addAccountToBalances convertedCurrency convertedAccount convertedBalances
              else pure convertedBalances
          actualBalances' <-
            if real -- Don't count virtual posting for this report
              then addAccountToBalances currency account actualBalances
              else pure actualBalances
          pure (convertedBalances', actualBalances')

  (mForBalancing, mActual) <- V.ifoldM incorporatePosting (M.empty, M.empty) transactionPostings
  let as = M.elems mForBalancing
  case MultiAccount.sum as of
    Nothing -> validationFailure $ BalanceErrorCouldNotSumPostings tl as
    Just d
      | d == MultiAccount.zero -> pure (Located tl mActual)
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance tl d $ V.toList transactionPostings

checkPercentage ::
  Eq ann =>
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
  (Located pctl (Percentage (Located rl ratio))) = do
    let Located pcl newCurrency = postingCurrency p
    when (newCurrency /= currency) $ validationFailure $ BalanceErrorPercentageCurrency tl pcl cl
    let Located pal previousAccount = postingAccount p
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
    -- We calculate Both P based on T and f
    -- and compare that to the actual P.
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
    (computedP, whereToRoundNext) <- case Account.fractionRatio Account.RoundNearest previousAccount computedPRatio of
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
          tl
          pl
          ppl
          pctl
          currency
          (Located al computedP)
          mComputedT
          mComputedRatio

unlines' :: [String] -> String
unlines' = intercalate "\n"
