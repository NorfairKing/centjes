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
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import Data.Validity (Validity (..))
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

newtype BalanceReport ann = BalanceReport
  {unBalanceReport :: Map AccountName (Money.MultiAccount (Currency ann))}
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (BalanceReport ann)

instance NFData ann => NFData (BalanceReport ann)

data BalanceError ann
  = BalanceErrorCouldNotAddTransaction !ann !AccountName !(Money.MultiAccount (Currency ann)) !(Money.MultiAccount (Currency ann))
  | BalanceErrorCouldNotAddPostings !ann !AccountName !ann !(Money.MultiAccount (Currency ann)) !(Money.MultiAccount (Currency ann))
  | BalanceErrorCouldNotSumPostings !ann ![Money.MultiAccount (Currency ann)]
  | BalanceErrorTransactionOffBalance !ann !(Money.MultiAccount (Currency ann))
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
                    [ [unwords ["Account: ", T.unpack (unAccountName an)]],
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
                  [ [unwords ["Account: ", T.unpack (unAccountName an)]],
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
    BalanceErrorTransactionOffBalance s ma ->
      Err
        (Just "BE_OFF_BALANCE")
        "Could not balance transaction"
        [ (toDiagnosePosition s, Where "While trying to balance this transaction"),
          (toDiagnosePosition s, This "This transaction is off balance."),
          (toDiagnosePosition s, Where $ unlines' ("By this amount:" : multiAccountLines ma))
          -- TODO make really good diagnostics if the transaction has only two postings
        ]
        []

multiAccountLines :: Money.MultiAccount (Currency ann) -> [String]
multiAccountLines = map go . M.toList . MultiAccount.unMultiAccount
  where
    go (c, a) =
      let Located _ qf = currencyQuantisationFactor c
       in unwords
            [ Account.format qf a,
              T.unpack $ unCurrencySymbol $ currencySymbol c
            ]

produceBalanceReport ::
  forall ann.
  Ord ann =>
  Ledger ann ->
  Validation (BalanceError ann) (BalanceReport ann)
produceBalanceReport m = do
  let incorporateAccount ::
        ann ->
        Map AccountName (Money.MultiAccount (Currency ann)) ->
        (AccountName, Money.MultiAccount (Currency ann)) ->
        Validation (BalanceError ann) (Map AccountName (Money.MultiAccount (Currency ann)))
      incorporateAccount l totals (an, current) = case M.lookup an totals of
        Nothing -> pure $ M.insert an current totals
        Just total -> case MultiAccount.add total current of
          Nothing -> validationFailure $ BalanceErrorCouldNotAddTransaction l an total current
          Just new -> pure $ M.insert an new totals
  let incorporateAccounts ::
        Map AccountName (Money.MultiAccount (Currency ann)) ->
        GenLocated ann (Map AccountName (Money.MultiAccount (Currency ann))) ->
        Validation (BalanceError ann) (Map AccountName (Money.MultiAccount (Currency ann)))
      incorporateAccounts totals (Located l currents) = foldM (incorporateAccount l) totals (M.toList currents)
  fmap BalanceReport $ traverse balanceTransaction (ledgerTransactions m) >>= foldM incorporateAccounts M.empty

balanceTransaction ::
  forall ann.
  Ord ann =>
  GenLocated ann (Transaction ann) ->
  Validation (BalanceError ann) (GenLocated ann (Map AccountName (Money.MultiAccount (Currency ann))))
balanceTransaction (Located tl Transaction {..}) = do
  let incorporatePosting ::
        Map AccountName (Money.MultiAccount (Currency ann)) ->
        GenLocated ann (Posting ann) ->
        Validation (BalanceError ann) (Map AccountName (Money.MultiAccount (Currency ann)))
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
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance tl d

unlines' :: [String] -> String
unlines' = intercalate "\n"
