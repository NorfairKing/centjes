{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Chart.Report
  ( ChartReport (..),
    ChartSeries (..),
    ChartError (..),
    produceChartReport,
    turnover,
    averageHeld,
    convertMultiAccountAtCost,
    chartReportIsNonNegative,
    sampleStepFunction,
    enumDays,
    renderChartReportText,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Convert (ConvertError (..), convertMultiAccountToAccount, lookupConversionRate, pricesToDailyPriceGraphs)
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import qualified Centjes.Filter as Filter
import Centjes.Ledger
import Centjes.Location
import Centjes.Report.EvaluatedLedger
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad (when)
import Data.List (sortOn, transpose)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Data.Traversable (for)
import Data.Validity (Validity (..), declare, decorateList, genericValidate)
import Data.Validity.Time ()
import Data.Validity.Vector ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.ConversionRate (ConversionRate)
import qualified Money.ConversionRate as ConversionRate
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Money.QuantisationFactor (QuantisationFactor)

-- | A backend-agnostic chart of one or more account balances over time,
-- already projected onto a single currency.
--
-- Every series' values are aligned to 'chartReportDays': for series @s@,
-- @chartSeriesValues s 'V.!' i@ is the balance on @chartReportDays 'V.!' i@.
data ChartReport ann = ChartReport
  { chartReportCurrency :: !(Currency ann),
    chartReportDays :: !(Vector Day),
    -- | In the order they are meant to be stacked, bottom first.
    chartReportSeries :: !(Vector (ChartSeries ann))
  }
  deriving (Show, Generic)

-- | The alignment promised in the haddock above is a real invariant: every
-- series carries exactly one value per day.
instance (Validity ann, Show ann, Ord ann) => Validity (ChartReport ann) where
  validate report =
    mconcat
      [ genericValidate report,
        decorateList (V.toList (chartReportSeries report)) $ \series ->
          declare "the series has exactly one value per day" $
            V.length (chartSeriesValues series) == V.length (chartReportDays report)
      ]

data ChartSeries ann = ChartSeries
  { chartSeriesAccount :: !AccountName,
    -- | Balances in 'chartReportCurrency', aligned to 'chartReportDays'.
    chartSeriesValues :: !(Vector Money.Account)
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (ChartSeries ann)

data ChartError ann
  = -- | The currency named in the settings is not declared in the ledger.
    ChartErrorUnknownCurrency !CurrencySymbol
  | -- | There is nothing to chart: no transactions matched, so there is no
    -- date range to draw.
    ChartErrorNoData
  | -- | A currency conversion failed while projecting onto the chart currency.
    ChartErrorConvert !(ConvertError ann)

instance ToReport (ChartError SourceSpan) where
  toReport = \case
    ChartErrorUnknownCurrency currencySymbol ->
      Err
        (Just "CHART_ERROR_UNKNOWN_CURRENCY")
        ("Cannot draw a chart in an undeclared currency: " <> CurrencySymbol.toString currencySymbol)
        []
        []
    ChartErrorNoData ->
      Err
        (Just "CHART_ERROR_NO_DATA")
        "There are no transactions to chart in the selected range and filter."
        []
        [Note "Widen the date range or relax the account filter."]
    -- Delegate to the conversion error's own, already-tested rendering.
    ChartErrorConvert convertError -> toReport convertError

-- | Turn an evaluated ledger into a chart report: for every account whose
-- declared type is among the given types and which passes the filter, the
-- cumulative (non-virtual) balance on every day in the ledger's range,
-- converted into the given currency.
--
-- The series come out ordered by ascending 'turnover', so the account that
-- money passes through slowest is stacked at the bottom.
produceChartReport ::
  forall ann.
  (Ord ann) =>
  -- | Which account types to chart (e.g. @[AccountTypeAssets]@).
  [AccountType] ->
  -- | An additional filter to narrow the selection further.
  Filter ->
  CurrencySymbol ->
  EvaluatedLedger ann ->
  Validation (ChartError ann) (ChartReport ann)
produceChartReport accountTypes accountFilter currencySymbol evaluatedLedger = do
  let ledger = evaluatedLedgerSource evaluatedLedger

  currency <-
    case M.lookup currencySymbol (ledgerCurrencies ledger) of
      Nothing -> validationFailure (ChartErrorUnknownCurrency currencySymbol)
      Just quantisationFactor -> pure (Currency currencySymbol quantisationFactor)

  -- An account is charted when its declared type is selected and it passes the
  -- filter.  Selecting by declared type (rather than by name) is what makes
  -- "the assets chart" mean assets, not "accounts whose name says assets".
  let isCharted :: AccountName -> Bool
      isCharted accountName =
        let declaredType = accountType . locatedValue <$> M.lookup accountName (ledgerAccounts ledger)
         in maybe False (`elem` accountTypes) declaredType
              && Filter.predicate accountFilter accountName

  -- For each transaction entry: its day and the selected cumulative balances
  -- (without virtual postings).  Later transactions on the same day overwrite
  -- earlier ones, so we keep the last balances seen on each day.
  let perDayBalances :: Map Day (AccountBalances ann)
      perDayBalances = V.foldl' insertEntry M.empty (evaluatedLedgerEntries evaluatedLedger)
        where
          insertEntry m = \case
            EvaluatedEntryPrice _ -> m
            EvaluatedEntryTransaction EvaluatedTransaction {..} ->
              let Located _ transaction = evaluatedTransactionLocated
                  Located _ timestamp = transactionTimestamp transaction
                  day = Timestamp.toDay timestamp
                  balances =
                    M.filterWithKey
                      (\accountName _ -> isCharted accountName)
                      evaluatedTransactionBalancesWithoutVirtual
               in M.insert day balances m

  days <-
    case NE.nonEmpty (M.keys perDayBalances) of
      Nothing -> validationFailure ChartErrorNoData
      Just dayKeys -> pure (enumDays (minimum dayKeys) (maximum dayKeys))

  -- Every account that ever appears after filtering.
  let accounts :: [AccountName]
      accounts = S.toList (S.unions (map M.keysSet (M.elems perDayBalances)))

  -- If nothing passes the filter there is no series to draw, which is just as
  -- much "no data" as a ledger without transactions.
  when (null accounts) $ validationFailure ChartErrorNoData

  -- The price graph in effect on each x-axis day, computed once and shared
  -- across every account rather than re-looked-up per account per day.
  let dailyPriceGraphs = pricesToDailyPriceGraphs (ledgerPrices ledger)
  let dayPriceGraphs :: Vector (MemoisedPriceGraph (Commodity ann))
      dayPriceGraphs =
        V.map (\day -> maybe MemoisedPriceGraph.empty snd (M.lookupLE day dailyPriceGraphs)) days

  -- A held balance is re-valued at each day's exchange rate, so a
  -- foreign-currency holding's line moves with the rate even on days without
  -- transactions.
  let convertWith ::
        MemoisedPriceGraph (Commodity ann) ->
        Money.MultiAccount (Commodity ann) ->
        Validation (ConvertError ann) Money.Account
      convertWith priceGraph =
        convertMultiAccountToAccount Nothing priceGraph currency

  let sampledPerAccount :: [(AccountName, Vector (Money.MultiAccount (Commodity ann)))]
      sampledPerAccount = flip map accounts $ \accountName ->
        -- The cumulative balance of this account on each day it changed.
        let stepMap = M.mapMaybe (M.lookup accountName) perDayBalances
         in (accountName, sampleStepFunction MultiAccount.zero days stepMap)

  -- Whatever prices are in force at the end, used for every day of the
  -- ordering key rather than each day's own, so that a price moving is not
  -- mistaken for money moving.
  let latestPriceGraph = maybe MemoisedPriceGraph.empty snd (M.lookupMax dailyPriceGraphs)

  let Located _ quantisationFactor = currencyQuantisationFactor currency

  -- A band low in the stack shifts every band above it, so the accounts that
  -- money passes through slowest belong at the bottom.  Ranking on how fast it
  -- passes through, rather than on how much or how often, is what lets a
  -- portfolio and a wallet be compared at all: one is counted in shares and the
  -- other in francs, but both turn over at some rate.
  --
  -- Among accounts that never turn over, the largest goes lowest, so that the
  -- chart stands on its biggest block and the slivers stay near the top.  The
  -- account name breaks the remaining ties so that the order is total.
  series <-
    mapValidationFailure ChartErrorConvert $ do
      keyed <- for sampledPerAccount $ \(accountName, sampled) -> do
        values <- V.zipWithM convertWith dayPriceGraphs sampled
        -- An account does not exist until it first holds something, so being
        -- opened is not money passing through it.
        let sinceOpened = V.dropWhile (== MultiAccount.zero) sampled
        costs <- traverse (convertMultiAccountAtCost latestPriceGraph currency) sinceOpened
        let held = V.map (Account.toRational quantisationFactor) costs
        pure
          ( (turnover held, negate (averageHeld held), accountName),
            ChartSeries {chartSeriesAccount = accountName, chartSeriesValues = values}
          )
      pure $ V.fromList $ map snd $ sortOn fst keyed

  pure
    ChartReport
      { chartReportCurrency = currency,
        chartReportDays = days,
        chartReportSeries = series
      }

-- | How fast money passes through an account: everything that flowed in or out
-- over the period, over the average amount held while it did.
--
-- The reciprocal is how long money stays put, which is the thing being ranked:
-- a current account turns over every few weeks, a pension turns over in
-- decades.  Because it is a ratio it says nothing about how big the account is,
-- so a wallet and a portfolio are compared on the same footing even though one
-- is counted in francs and the other in shares.
turnover :: Vector Rational -> Rational
turnover values =
  let held = averageHeld values
   in if held == 0
        then 0
        else sum (V.map abs (V.zipWith subtract values (V.drop 1 values))) / held

-- | The average of what an account held over the period, which is the
-- denominator that makes 'turnover' a rate rather than an amount.
averageHeld :: Vector Rational -> Rational
averageHeld values
  | V.null values = 0
  | otherwise = sum (V.map abs values) / toRational (V.length values)

-- | What the holdings cost, rather than what they are worth now: a lot counts
-- at the rate it was bought at, and every currency at whichever prices the
-- given graph holds.
--
-- Handing this one fixed graph is what keeps prices out of the ordering.
-- Neither an exchange rate moving nor a share price running changes what a
-- holding cost, and buying a share with cash the account already held leaves
-- its cost untouched, so neither shows up as money passing through.
convertMultiAccountAtCost ::
  forall ann.
  (Ord ann) =>
  MemoisedPriceGraph (Commodity ann) ->
  Currency ann ->
  Money.MultiAccount (Commodity ann) ->
  Validation (ConvertError ann) Money.Account
convertMultiAccountAtCost priceGraph currencyTo multiAccount = do
  let costRate :: Commodity ann -> Validation (ConvertError ann) (ConversionRate, QuantisationFactor)
      costRate = \case
        commodity@(CommodityCurrency _) ->
          lookupConversionRate Nothing priceGraph currencyTo commodity
        commodity@(CommodityLot lot) -> do
          (rate, _) <-
            lookupConversionRate Nothing priceGraph currencyTo (CommodityCurrency (lotBasisCurrency lot))
          pure
            ( ConversionRate.compose (lotBasisRate lot) rate,
              locatedValue (commodityQuantisationFactor commodity)
            )
  (mResult, _) <-
    MultiAccount.convertAllA
      MultiAccount.RoundNearest
      (locatedValue (currencyQuantisationFactor currencyTo))
      costRate
      multiAccount
  case mResult of
    Nothing -> validationFailure (ConvertErrorInvalidSum currencyTo)
    Just result -> pure result

-- | Sample a step function at each day on the (ascending) x-axis.
--
-- For each day, take the value attached to the greatest key @<= day@; before
-- the first known key, use the given default.
sampleStepFunction ::
  -- | Value before the first known change.
  v ->
  -- | x-axis, ascending.
  Vector Day ->
  -- | Values keyed by the day they take effect.
  Map Day v ->
  Vector v
sampleStepFunction before days known =
  V.map (\day -> maybe before snd (M.lookupLE day known)) days

-- | All days in the inclusive range, ascending.
enumDays :: Day -> Day -> Vector Day
enumDays from to = V.fromList [from .. to]

-- | Whether every value in the report is non-negative.
--
-- A stacked area chart only makes sense for non-negative series; a renderer
-- can use this to fall back to a different chart type for signed data (e.g. an
-- account that goes into overdraft).
chartReportIsNonNegative :: ChartReport ann -> Bool
chartReportIsNonNegative =
  all (all (>= Account.zero) . chartSeriesValues) . chartReportSeries

-- | A deterministic, line-oriented rendering of a chart report, for golden
-- testing.  One header line, then one tab-separated line per day.
renderChartReportText :: ChartReport ann -> Text
renderChartReportText ChartReport {..} =
  let Located _ quantisationFactor = currencyQuantisationFactor chartReportCurrency
      seriesList = V.toList chartReportSeries
      header =
        T.intercalate "\t" $
          "day" : map (T.pack . AccountName.toString . chartSeriesAccount) seriesList
      -- One value column per series; transpose to one row per day, so no
      -- partial indexing is needed and a misaligned series degrades gracefully.
      valueRows = transpose (map (V.toList . chartSeriesValues) seriesList)
      renderRow day values =
        T.intercalate "\t" $
          T.pack (show day) : map (T.pack . Account.format quantisationFactor) values
      rows = zipWith renderRow (V.toList chartReportDays) valueRows
   in T.unlines (header : rows)
