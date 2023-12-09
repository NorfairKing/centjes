{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Balance
  ( BalanceReport (..),
    BalanceError (..),
    produceBalanceReport,
  )
where

import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Control.DeepSeq
import Control.Monad
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Traversable
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Numeric.DecimalLiteral as DecimalLiteral

newtype BalancedLedger ann = BalancedLedger {balancedLedgerTransactions :: Vector (GenLocated ann (Transaction ann), AccountBalances ann)}
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalancedLedger ann)

instance NFData ann => NFData (BalancedLedger ann)

type AccountBalances ann = Map AccountName (Money.MultiAccount (Currency ann))

newtype BalanceReport ann = BalanceReport
  {unBalanceReport :: AccountBalances ann}
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceReport ann)

instance NFData ann => NFData (BalanceReport ann)

data BalanceError ann
  = BalanceErrorCouldNotAddTransaction !ann !AccountName !(Money.MultiAccount (Currency ann)) !(Money.MultiAccount (Currency ann))
  | BalanceErrorCouldNotAddPostings !ann !AccountName !ann !(Money.MultiAccount (Currency ann)) !(Money.MultiAccount (Currency ann))
  | BalanceErrorCouldNotSumPostings !ann ![Money.MultiAccount (Currency ann)]
  | BalanceErrorTransactionOffBalance !ann !(Money.MultiAccount (Currency ann)) ![GenLocated ann (Posting ann)]
  | BalanceErrorAssertion !ann !(GenLocated ann (Assertion ann)) !(Money.MultiAccount (Currency ann)) !(Maybe Money.Account)
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
                    [ [unwords ["Account: ", T.unpack (accountNameText an)]],
                      ["Running total:"],
                      multiAccountLines subtotal,
                      ["Balance to add:"],
                      multiAccountLines current
                    ]
              )
          )
        ]
        []
    BalanceErrorCouldNotAddPostings st an sp ma1 ma2 ->
      Err
        (Just "BE_ACCOUNT_TOTAL")
        "Could not add postings to compute the total amount"
        [ (toDiagnosePosition st, Where "While trying to balance this transaction"),
          (toDiagnosePosition st, This "Could not add these amounts together because the result would become too big:"),
          ( toDiagnosePosition st,
            Where $
              unlines' $
                concat
                  [ [unwords ["Account: ", T.unpack (accountNameText an)]],
                    ["Running total:"],
                    multiAccountLines ma1
                  ]
          ),
          (toDiagnosePosition sp, Where "while trying to incorporate this posting"),
          ( toDiagnosePosition sp,
            Where $
              unlines' $
                concat
                  [ ["Amount to add:"],
                    multiAccountLines ma2
                  ]
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
          suggestedLiteral <- DecimalLiteral.fromAccount qf suggestedAccount
          let symbol = currencySymbol currency
          pure
            ( toDiagnosePosition $ combineSpans al cl,
              Maybe $
                unwords
                  [ "Perhaps you meant",
                    renderDecimalLiteral suggestedLiteral,
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
  Ledger ann ->
  Validation (BalanceError ann) (BalanceReport ann)
produceBalanceReport l =
  ( \bl ->
      let v = balancedLedgerTransactions bl
       in BalanceReport $
            if V.null v
              then M.empty
              else snd (V.last v)
  )
    <$> produceBalancedLedger l

produceBalancedLedger ::
  forall ann.
  Ord ann =>
  Ledger ann ->
  Validation (BalanceError ann) (BalancedLedger ann)
produceBalancedLedger ledger = do
  tups <- for (ledgerTransactions ledger) $ \t -> (,) t <$> balanceTransaction t

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
        AccountBalances ann ->
        GenLocated ann (Posting ann) ->
        Validation (BalanceError ann) (AccountBalances ann)
      incorporatePosting m (Located _ (Posting (Located pl an) (Located _ currency) (Located _ account))) =
        let current = MultiAccount.fromAccount currency account
         in case M.lookup an m of
              Nothing -> pure $ M.insert an current m
              Just total -> case MultiAccount.add total current of
                Nothing -> validationFailure $ BalanceErrorCouldNotAddPostings tl an pl total current
                Just acc'' -> pure $ M.insert an acc'' m
  m <- foldM incorporatePosting M.empty transactionPostings
  let as = M.elems m
  case MultiAccount.sum as of
    Nothing -> validationFailure $ BalanceErrorCouldNotSumPostings tl as
    Just d
      | d == MultiAccount.zero -> pure (Located tl m)
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance tl d $ V.toList transactionPostings

unlines' :: [String] -> String
unlines' = intercalate "\n"
