{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Report.Balance
  ( BalanceReport (..),
    BalanceError (..),
    produceBalanceReport,
  )
where

import Centjes.Ledger
import Centjes.Validation
import Control.Exception
import Control.Monad
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Validity (Validity (..))
import GHC.Generics (Generic)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

newtype BalanceReport = BalanceReport
  {unBalanceReport :: Map AccountName (Money.MultiAccount Currency)}
  deriving (Show, Eq, Generic)

instance Validity BalanceReport

data BalanceError
  = BalanceErrorCouldNotAdd !(Money.MultiAccount Currency) !(Money.MultiAccount Currency)
  | BalanceErrorCouldNotSum ![Money.MultiAccount Currency]
  | BalanceErrorTransactionOffBalance !Transaction !(Money.MultiAccount Currency)
  deriving stock (Show, Eq, Generic)

instance Exception BalanceError where
  displayException = show -- TODO

produceBalanceReport :: Ledger -> Validation BalanceError BalanceReport
produceBalanceReport m = do
  let incorporateAccounts ::
        Map AccountName (Money.MultiAccount Currency) ->
        Map AccountName (Money.MultiAccount Currency) ->
        Validation BalanceError (Map AccountName (Money.MultiAccount Currency))
      incorporateAccounts totals current =
        traverse
          ( \case
              Left (a1, a2) -> validationFailure $ BalanceErrorCouldNotAdd a1 a2
              Right a -> pure a
          )
          $ M.unionWith
            ( \ea1 ea2 -> do
                a1 <- ea1
                a2 <- ea2
                case MultiAccount.add a1 a2 of
                  Nothing -> Left (a1, a2)
                  Just a -> Right a
            )
            (M.map Right totals)
            (M.map Right current)
  fmap BalanceReport $ mapM balanceTransaction (ledgerTransactions m) >>= foldM incorporateAccounts M.empty

balanceTransaction :: Transaction -> Validation BalanceError (Map AccountName (Money.MultiAccount Currency))
balanceTransaction t@Transaction {..} = do
  let incorporatePosting ::
        Map AccountName (Money.MultiAccount Currency) ->
        Posting ->
        Validation BalanceError (Map AccountName (Money.MultiAccount Currency))
      incorporatePosting m (Posting an currency account) =
        let acc = MultiAccount.fromAccount currency account
         in case M.lookup an m of
              Nothing -> pure $ M.insert an acc m
              Just acc' -> case MultiAccount.add acc acc' of
                Nothing -> validationFailure $ BalanceErrorCouldNotAdd acc acc'
                Just acc'' -> pure $ M.insert an acc'' m
  m <- foldM incorporatePosting M.empty transactionPostings
  let as = M.elems m
  case MultiAccount.sum as of
    Nothing -> validationFailure $ BalanceErrorCouldNotSum as
    Just d
      | d == MultiAccount.zero -> pure m
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance t d
