{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Balance
  ( BalanceReport (..),
    produceBalanceReportFromEvaluatedLedger,
    -- Re-exports from EvaluatedLedger
    AccountBalances,
    BalanceError (..),
  )
where

import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import Centjes.Ledger
import Centjes.Location
import Centjes.Report.EvaluatedLedger
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import qualified Data.Map.Strict as M
import Data.Time
import Data.Validity hiding (Validation (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

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

-- | Produce a balance report from a pre-computed evaluated ledger.
--
-- This extracts the final account balances from the evaluated ledger
-- (optionally truncated to an end date), filters and converts them,
-- and returns a 'BalanceReport'.
produceBalanceReportFromEvaluatedLedger ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Maybe Day ->
  Maybe CurrencySymbol ->
  Bool ->
  EvaluatedLedger ann ->
  Validation (BalanceError ann) (BalanceReport ann)
produceBalanceReportFromEvaluatedLedger f mEnd mCurrencySymbolTo showVirtual evaluatedLedger = do
  let entries = evaluatedLedgerEntries evaluatedLedger
      ledger = evaluatedLedgerSource evaluatedLedger

  -- Truncate entries to end date
  let truncatedEntries = case mEnd of
        Nothing -> entries
        Just end -> V.takeWhile (entryBeforeOrOnDay end) entries

  -- Find the last entry and extract balances + price graph
  let (finalBalances, finalPriceGraph) = case lastEntryState showVirtual truncatedEntries of
        Nothing -> (M.empty, MemoisedPriceGraph.empty)
        Just (bal, pg) -> (bal, pg)

  let balances = filterAccountBalances f finalBalances

  -- Convert currencies if a target currency is specified
  balanceReportBalances <- mapValidationFailure BalanceErrorConvertError $ case mCurrencySymbolTo of
    Nothing -> pure balances
    Just currencySymbolTo -> do
      currencyTo <- lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
      convertAccountBalances
        finalPriceGraph
        currencyTo
        balances

  balanceReportFilledBalances <- fillAccountBalances balanceReportBalances

  balanceReportTotal <- case MultiAccount.sum balanceReportBalances of
    Nothing -> validationFailure $ BalanceErrorCouldNotSumTotal $ M.elems balanceReportBalances
    Just total -> pure total

  pure BalanceReport {..}

-- | Check if an evaluated entry is on or before the given day.
entryBeforeOrOnDay :: Day -> EvaluatedEntry ann -> Bool
entryBeforeOrOnDay end = \case
  EvaluatedEntryTransaction evaluatedTransaction ->
    let Located _ t = evaluatedTransactionLocated evaluatedTransaction
        Located _ ts = transactionTimestamp t
     in Timestamp.toDay ts <= end
  EvaluatedEntryPrice evaluatedPrice ->
    let Located _ p = evaluatedPriceLocated evaluatedPrice
        Located _ ts = priceTimestamp p
     in Timestamp.toDay ts <= end

-- | Get the final account balances and price graph from the last entry in a vector.
--
-- For a transaction entry, uses the last posting's balances.
-- For a price entry, uses the price's balances.
lastEntryState ::
  (Ord ann) =>
  Bool ->
  Vector (EvaluatedEntry ann) ->
  Maybe (AccountBalances ann, MemoisedPriceGraph (Currency ann))
lastEntryState showVirtual entries
  | V.null entries = Nothing
  | otherwise = Just $ case V.last entries of
      EvaluatedEntryTransaction evaluatedTransaction ->
        let postings = evaluatedTransactionPostings evaluatedTransaction
            pickBalance = if showVirtual then evaluatedPostingBalancesWithVirtual else evaluatedPostingBalancesWithoutVirtual
            balance =
              if V.null postings
                then if showVirtual then evaluatedTransactionBalancesWithVirtual evaluatedTransaction else evaluatedTransactionBalancesWithoutVirtual evaluatedTransaction
                else pickBalance (V.last postings)
            rawPriceGraph =
              if V.null postings
                then evaluatedTransactionPriceGraph evaluatedTransaction
                else evaluatedPostingPriceGraph (V.last postings)
         in (balance, MemoisedPriceGraph.fromPriceGraph rawPriceGraph)
      EvaluatedEntryPrice evaluatedPrice ->
        let balance = if showVirtual then evaluatedPriceBalancesWithVirtual evaluatedPrice else evaluatedPriceBalancesWithoutVirtual evaluatedPrice
         in (balance, MemoisedPriceGraph.fromPriceGraph (evaluatedPricePriceGraph evaluatedPrice))
