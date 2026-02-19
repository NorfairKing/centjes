{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.NetWorth
  ( NetWorthReport (..),
    NetWorthReportConvertedData (..),
    NetWorthReportMultiCurrencyData (..),
    NetWorthError (..),
    produceNetWorthReport,
  )
where

import Centjes.AccountType
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Ledger
import Centjes.Location
import Centjes.Report.Balance (BalanceReport (..))
import Centjes.Report.EvaluatedLedger
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import qualified Money.Account as Money (Account)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

data NetWorthReport ann
  = NetWorthReportConverted !(NetWorthReportConvertedData ann)
  | NetWorthReportMultiCurrency !(NetWorthReportMultiCurrencyData ann)
  deriving (Show)

data NetWorthReportConvertedData ann = NetWorthReportConvertedData
  { netWorthReportConvertedCurrency :: !(Currency ann),
    netWorthReportConvertedEntries :: !(Vector (Day, Money.Account)),
    netWorthReportConvertedBalanceReport :: !(Maybe (BalanceReport ann))
  }
  deriving (Show)

data NetWorthReportMultiCurrencyData ann = NetWorthReportMultiCurrencyData
  { netWorthReportMultiCurrencyEntries :: !(Vector (Day, Money.MultiAccount (Currency ann))),
    netWorthReportMultiCurrencyBalanceReport :: !(Maybe (BalanceReport ann))
  }
  deriving (Show)

data NetWorthError ann
  = NetWorthErrorEvaluatedLedger !(EvaluatedLedgerError ann)
  | NetWorthErrorConvert !(ConvertError ann)
  | NetWorthErrorCouldNotSum !Day
  | NetWorthErrorBalance !(BalanceError ann)

instance ToReport (NetWorthError SourceSpan) where
  toReport = \case
    NetWorthErrorEvaluatedLedger evaluatedLedgerError -> toReport evaluatedLedgerError
    NetWorthErrorConvert convertError -> toReport convertError
    NetWorthErrorCouldNotSum day ->
      Err
        (Just "NW_SUM")
        ("Could not sum account balances for net worth on " <> show day)
        []
        []
    NetWorthErrorBalance balanceError -> toReport balanceError

produceNetWorthReport ::
  forall ann.
  (Ord ann) =>
  Maybe CurrencySymbol ->
  Maybe Day ->
  Maybe Day ->
  Ledger ann ->
  Validation (NetWorthError ann) (NetWorthReport ann)
produceNetWorthReport mCurrencySymbolTo mBegin mEnd ledger = do
  -- Produce evaluated ledger
  evaluatedLedger <-
    mapValidationFailure NetWorthErrorEvaluatedLedger $
      produceEvaluatedLedger ledger

  case mCurrencySymbolTo of
    Just currencySymbolTo -> do
      -- Resolve target currency
      currency <-
        mapValidationFailure NetWorthErrorConvert $
          lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
      NetWorthReportConverted <$> produceConvertedReport currency mBegin mEnd evaluatedLedger
    Nothing ->
      NetWorthReportMultiCurrency <$> produceMultiCurrencyReport mBegin mEnd evaluatedLedger

produceConvertedReport ::
  forall ann.
  (Ord ann) =>
  Currency ann ->
  Maybe Day ->
  Maybe Day ->
  EvaluatedLedger ann ->
  Validation (NetWorthError ann) (NetWorthReportConvertedData ann)
produceConvertedReport netWorthReportConvertedCurrency mBegin mEnd evaluatedLedger = do
  let ledger = evaluatedLedgerSource evaluatedLedger
      entries = evaluatedLedgerEntries evaluatedLedger

  -- Build day-boundary snapshots of (balances, priceGraph) from entries
  let daySnapshots = buildDaySnapshots (ledgerAccounts ledger) entries

  if M.null daySnapshots
    then pure $ NetWorthReportConvertedData {netWorthReportConvertedEntries = V.empty, netWorthReportConvertedBalanceReport = Nothing, ..}
    else do
      -- Determine date range
      let firstDay = fst (M.findMin daySnapshots)
      let lastDay = fst (M.findMax daySnapshots)
      let beginDay = fromMaybe firstDay mBegin
      let endDay = fromMaybe lastDay mEnd

      -- Generate entries for each day in the range
      netWorthReportConvertedEntries <-
        V.fromList
          <$> traverse
            ( \day -> do
                let (balances, priceGraph) = fromMaybe (M.empty, MemoisedPriceGraph.empty) $ lookupLE day daySnapshots
                account <- computeDayNetWorth day priceGraph netWorthReportConvertedCurrency balances
                pure (day, account)
            )
            [beginDay .. endDay]

      -- Produce balance report from the final day's snapshot
      let (finalBalances, finalPriceGraph) = fromMaybe (M.empty, MemoisedPriceGraph.empty) $ lookupLE endDay daySnapshots
      balanceReportBalances <-
        mapValidationFailure NetWorthErrorConvert $
          convertAccountBalances finalPriceGraph netWorthReportConvertedCurrency finalBalances
      balanceReportFilledBalances <-
        mapValidationFailure NetWorthErrorBalance $
          fillAccountBalances balanceReportBalances
      balanceReportTotal <- case MultiAccount.sum balanceReportBalances of
        Nothing -> validationFailure $ NetWorthErrorCouldNotSum endDay
        Just total -> pure total
      let netWorthReportConvertedBalanceReport = Just BalanceReport {..}

      pure NetWorthReportConvertedData {..}

produceMultiCurrencyReport ::
  forall ann.
  (Ord ann) =>
  Maybe Day ->
  Maybe Day ->
  EvaluatedLedger ann ->
  Validation (NetWorthError ann) (NetWorthReportMultiCurrencyData ann)
produceMultiCurrencyReport mBegin mEnd evaluatedLedger = do
  let ledger = evaluatedLedgerSource evaluatedLedger
      entries = evaluatedLedgerEntries evaluatedLedger

  -- Build day-boundary snapshots of (balances, priceGraph) from entries
  let daySnapshots = buildDaySnapshots (ledgerAccounts ledger) entries

  if M.null daySnapshots
    then
      pure $
        NetWorthReportMultiCurrencyData
          { netWorthReportMultiCurrencyEntries = V.empty,
            netWorthReportMultiCurrencyBalanceReport = Nothing
          }
    else do
      -- Determine date range
      let firstDay = fst (M.findMin daySnapshots)
      let lastDay = fst (M.findMax daySnapshots)
      let beginDay = fromMaybe firstDay mBegin
      let endDay = fromMaybe lastDay mEnd

      -- Generate entries for each day in the range
      netWorthReportMultiCurrencyEntries <-
        V.fromList
          <$> traverse
            ( \day -> do
                let (balances, _priceGraph) = fromMaybe (M.empty, MemoisedPriceGraph.empty) $ lookupLE day daySnapshots
                multiAccount <- computeDayMultiCurrencyNetWorth day balances
                pure (day, multiAccount)
            )
            [beginDay .. endDay]

      -- Produce unconverted balance report from the final day's snapshot
      let (finalBalances, _finalPriceGraph) = fromMaybe (M.empty, MemoisedPriceGraph.empty) $ lookupLE endDay daySnapshots
      let balanceReportBalances = finalBalances
      balanceReportFilledBalances <-
        mapValidationFailure NetWorthErrorBalance $
          fillAccountBalances balanceReportBalances
      balanceReportTotal <- case MultiAccount.sum balanceReportBalances of
        Nothing -> validationFailure $ NetWorthErrorCouldNotSum endDay
        Just total -> pure total
      let netWorthReportMultiCurrencyBalanceReport = Just BalanceReport {..}

      pure NetWorthReportMultiCurrencyData {..}

-- | Build a map from Day to (asset+liability balances, price graph) snapshots.
--
-- Only transaction entries create snapshots (determining the date range).
-- Price-only days are not included as they don't change balances.
-- The price graph used is the one from the transaction's evaluated state,
-- which already incorporates all prices declared on or before that day.
buildDaySnapshots ::
  (Ord ann) =>
  Map AccountName (GenLocated ann (Account ann)) ->
  Vector (EvaluatedEntry ann) ->
  Map Day (AccountBalances ann, MemoisedPriceGraph (Currency ann))
buildDaySnapshots accounts = V.foldl' go M.empty
  where
    go m = \case
      EvaluatedEntryTransaction evaluatedTransaction ->
        let Located _ t = evaluatedTransactionLocated evaluatedTransaction
            Located _ ts = transactionTimestamp t
            day = Timestamp.toDay ts
            balances = evaluatedTransactionBalancesWithoutVirtual evaluatedTransaction
            priceGraph = MemoisedPriceGraph.fromPriceGraph (evaluatedTransactionPriceGraph evaluatedTransaction)
            filtered = M.filterWithKey isAssetOrLiability balances
         in M.insert day (filtered, priceGraph) m
      EvaluatedEntryPrice _ -> m
    isAssetOrLiability an _ = case M.lookup an accounts of
      Nothing -> False
      Just (Located _ acc) -> case accountType acc of
        AccountTypeAssets -> True
        AccountTypeLiabilities -> True
        _ -> False

-- | Compute the net worth for a single day by summing all account balances and converting to the target currency.
computeDayNetWorth ::
  (Ord ann) =>
  Day ->
  MemoisedPriceGraph (Currency ann) ->
  Currency ann ->
  AccountBalances ann ->
  Validation (NetWorthError ann) Money.Account
computeDayNetWorth day priceGraph currencyTo balances = do
  case MultiAccount.sum (M.elems balances) of
    Nothing -> validationFailure $ NetWorthErrorCouldNotSum day
    Just total ->
      mapValidationFailure NetWorthErrorConvert $
        convertMultiAccountToAccount Nothing priceGraph currencyTo total

-- | Compute the net worth for a single day by summing all account balances without converting.
computeDayMultiCurrencyNetWorth ::
  (Ord ann) =>
  Day ->
  AccountBalances ann ->
  Validation (NetWorthError ann) (Money.MultiAccount (Currency ann))
computeDayMultiCurrencyNetWorth day balances =
  case MultiAccount.sum (M.elems balances) of
    Nothing -> validationFailure $ NetWorthErrorCouldNotSum day
    Just total -> pure total

-- | Look up the greatest key less than or equal to the given key.
lookupLE :: (Ord k) => k -> Map k v -> Maybe v
lookupLE k m = snd <$> M.lookupLE k m
