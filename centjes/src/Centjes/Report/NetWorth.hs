{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.NetWorth
  ( NetWorthReport (..),
    NetWorthError (..),
    produceNetWorthReport,
  )
where

import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Ledger
import Centjes.Location
import Centjes.Report.Balance
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
import qualified Money.MultiAccount as MultiAccount

data NetWorthReport ann = NetWorthReport
  { netWorthReportCurrency :: !(Currency ann),
    netWorthReportEntries :: !(Vector (Day, Money.Account))
  }
  deriving (Show)

data NetWorthError ann
  = NetWorthErrorBalance !(BalanceError ann)
  | NetWorthErrorConvert !(ConvertError ann)
  | NetWorthErrorCouldNotSum !Day

instance ToReport (NetWorthError SourceSpan) where
  toReport = \case
    NetWorthErrorBalance be -> toReport be
    NetWorthErrorConvert ce -> toReport ce
    NetWorthErrorCouldNotSum day ->
      Err
        (Just "NW_SUM")
        ("Could not sum account balances for net worth on " <> show day)
        []
        []

produceNetWorthReport ::
  forall ann.
  (Ord ann) =>
  CurrencySymbol ->
  Maybe Day ->
  Maybe Day ->
  Ledger ann ->
  Validation (NetWorthError ann) (NetWorthReport ann)
produceNetWorthReport currencySymbolTo mBegin mEnd ledger = do
  -- Resolve target currency
  netWorthReportCurrency <-
    mapValidationFailure NetWorthErrorConvert $
      lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo

  -- Produce balanced ledger (no virtual postings)
  balancedLedger <-
    mapValidationFailure NetWorthErrorBalance $
      produceBalancedLedger False ledger

  let transactions = balancedLedgerTransactions balancedLedger

  if V.null transactions
    then pure $ NetWorthReport {netWorthReportEntries = V.empty, ..}
    else do
      -- Determine date range from transactions
      let firstDay = transactionDay (fst (V.head transactions))
      let lastDay = transactionDay (fst (V.last transactions))
      let beginDay = fromMaybe firstDay mBegin
      let endDay = fromMaybe lastDay mEnd

      -- Build daily price graphs
      let dailyPriceGraphs = pricesToDailyPriceGraphs (ledgerPrices ledger)

      -- Build daily account balances (only assets and liabilities)
      let dayBalances = buildDayBalances (ledgerAccounts ledger) transactions

      -- Generate entries for each day in the range
      netWorthReportEntries <-
        V.fromList
          <$> traverse
            ( \day -> do
                let balances = fromMaybe M.empty $ lookupLE day dayBalances
                let priceGraph = fromMaybe MemoisedPriceGraph.empty $ lookupLE day dailyPriceGraphs
                account <- computeDayNetWorth day priceGraph netWorthReportCurrency balances
                pure (day, account)
            )
            [beginDay .. endDay]

      pure NetWorthReport {..}

transactionDay :: GenLocated ann (Transaction ann) -> Day
transactionDay (Located _ t) =
  let Located _ ts = transactionTimestamp t
   in Timestamp.toDay ts

-- | Build a map from Day to the cumulative balances of asset and liability accounts as of that day.
buildDayBalances ::
  Map AccountName (GenLocated ann (Account ann)) ->
  Vector (GenLocated ann (Transaction ann), AccountBalances ann) ->
  Map Day (AccountBalances ann)
buildDayBalances accounts = V.foldl' go M.empty
  where
    go m (lt, balances) =
      let day = transactionDay lt
          filtered = M.filterWithKey isAssetOrLiability balances
       in M.insert day filtered m
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

-- | Look up the greatest key less than or equal to the given key.
lookupLE :: (Ord k) => k -> Map k v -> Maybe v
lookupLE k m = snd <$> M.lookupLE k m
