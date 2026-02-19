{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.IncomeStatement
  ( IncomeStatementReport (..),
    IncomeStatementError (..),
    produceIncomeStatementReport,
  )
where

import Centjes.AccountType
import Centjes.Convert
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import Centjes.Ledger
import Centjes.Location
import Centjes.Report.Balance (entryBeforeOrOnDay, lastEntryState)
import Centjes.Report.EvaluatedLedger
import Centjes.Validation
import qualified Data.Map.Strict as M
import Data.Time
import Data.Validity hiding (Validation (..))
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

data IncomeStatementReport ann = IncomeStatementReport
  { incomeStatementReportIncome :: !(AccountBalances ann),
    incomeStatementReportFilledIncome :: !(AccountBalances ann),
    incomeStatementReportTotalIncome :: !(Money.MultiAccount (Currency ann)),
    incomeStatementReportExpenses :: !(AccountBalances ann),
    incomeStatementReportFilledExpenses :: !(AccountBalances ann),
    incomeStatementReportTotalExpenses :: !(Money.MultiAccount (Currency ann)),
    -- | Unrealized exchange gains/losses from currency conversion.
    -- This is the difference between the change in net worth (assets + liabilities)
    -- and the sum of income + expenses. Only meaningful when currency conversion is used.
    incomeStatementReportRevaluation :: !(Maybe (Money.MultiAccount (Currency ann))),
    incomeStatementReportNetIncome :: !(Money.MultiAccount (Currency ann))
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (IncomeStatementReport ann)

data IncomeStatementError ann
  = IncomeStatementErrorConvert !(ConvertError ann)
  | IncomeStatementErrorBalance !(BalanceError ann)
  | IncomeStatementErrorCouldNotSum
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (IncomeStatementError ann)

instance ToReport (IncomeStatementError SourceSpan) where
  toReport = \case
    IncomeStatementErrorConvert ce -> toReport ce
    IncomeStatementErrorBalance be -> toReport be
    IncomeStatementErrorCouldNotSum ->
      Err
        (Just "IS_SUM")
        "Could not sum account balances for income statement."
        []
        []

produceIncomeStatementReport ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Maybe Day ->
  Maybe Day ->
  Maybe CurrencySymbol ->
  Bool ->
  EvaluatedLedger ann ->
  Validation (IncomeStatementError ann) (IncomeStatementReport ann)
produceIncomeStatementReport f mBegin mEnd mCurrencySymbolTo showVirtual evaluatedLedger = do
  let entries = evaluatedLedgerEntries evaluatedLedger
      ledger = evaluatedLedgerSource evaluatedLedger
      accounts = ledgerAccounts ledger

  -- Truncate entries to end date
  let truncatedEntries = case mEnd of
        Nothing -> entries
        Just end -> V.takeWhile (entryBeforeOrOnDay end) entries

  -- Get balances at end date
  let (endBalances, endPriceGraph) = case lastEntryState showVirtual truncatedEntries of
        Nothing -> (M.empty, MemoisedPriceGraph.empty)
        Just (bal, pg) -> (bal, pg)

  -- Get balances at begin date (day before begin, or empty if no begin)
  let (beginBalances, beginPriceGraph) = case mBegin of
        Nothing -> (M.empty, MemoisedPriceGraph.empty)
        Just begin ->
          let beforeBegin = V.takeWhile (entryBeforeOrOnDay (addDays (-1) begin)) entries
           in case lastEntryState showVirtual beforeBegin of
                Nothing -> (M.empty, MemoisedPriceGraph.empty)
                Just (bal, pg) -> (bal, pg)

  -- Compute period activity: endBalances - beginBalances per account
  let allAccountNames = M.keys endBalances ++ M.keys beginBalances
      periodBalances = M.fromList $ do
        an <- allAccountNames
        let endBal = M.findWithDefault MultiAccount.zero an endBalances
            beginBal = M.findWithDefault MultiAccount.zero an beginBalances
        case MultiAccount.subtract endBal beginBal of
          Nothing -> [] -- Should not happen with valid data; skip if it does
          Just diff -> [(an, diff)]

  -- Split into income and expense accounts
  let isIncomeAccount an = case M.lookup an accounts of
        Nothing -> False
        Just (Located _ acc) -> accountType acc == AccountTypeIncome
      isExpensesAccount an = case M.lookup an accounts of
        Nothing -> False
        Just (Located _ acc) -> accountType acc == AccountTypeExpenses

  let incomeBalances = filterAccountBalances f $ M.filterWithKey (\an _ -> isIncomeAccount an) periodBalances
      expensesBalances = filterAccountBalances f $ M.filterWithKey (\an _ -> isExpensesAccount an) periodBalances

  -- Convert currencies if a target currency is specified
  incomeStatementReportIncome <- mapValidationFailure IncomeStatementErrorConvert $ case mCurrencySymbolTo of
    Nothing -> pure incomeBalances
    Just currencySymbolTo -> do
      currencyTo <- lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
      convertAccountBalances
        endPriceGraph
        currencyTo
        incomeBalances

  incomeStatementReportExpenses <- mapValidationFailure IncomeStatementErrorConvert $ case mCurrencySymbolTo of
    Nothing -> pure expensesBalances
    Just currencySymbolTo -> do
      currencyTo <- lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
      convertAccountBalances
        endPriceGraph
        currencyTo
        expensesBalances

  -- Fill parent accounts
  incomeStatementReportFilledIncome <-
    mapValidationFailure IncomeStatementErrorBalance $
      fillAccountBalances incomeStatementReportIncome

  incomeStatementReportFilledExpenses <-
    mapValidationFailure IncomeStatementErrorBalance $
      fillAccountBalances incomeStatementReportExpenses

  -- Compute totals
  incomeStatementReportTotalIncome <- case MultiAccount.sum incomeStatementReportIncome of
    Nothing -> validationFailure IncomeStatementErrorCouldNotSum
    Just total -> pure total

  incomeStatementReportTotalExpenses <- case MultiAccount.sum incomeStatementReportExpenses of
    Nothing -> validationFailure IncomeStatementErrorCouldNotSum
    Just total -> pure total

  -- Compute revaluation: unrealized exchange gains/losses when converting currencies.
  -- Revaluation = -(net worth change in target currency) - (income + expenses in target currency)
  -- where net worth change = end_net_worth(at end rates) - begin_net_worth(at begin rates).
  -- This captures exchange rate effects: holding assets in foreign currencies
  -- that change value due to rate movements.
  -- Only computed when currency conversion is used.
  incomeStatementReportRevaluation <- case mCurrencySymbolTo of
    Nothing -> pure Nothing
    Just currencySymbolTo -> do
      currencyTo <-
        mapValidationFailure IncomeStatementErrorConvert $
          lookupConversionCurrency (ledgerCurrencies ledger) currencySymbolTo
      -- Compute end net worth (assets + liabilities) converted at end rates
      let endNetWorthBalances = filterAssetAndLiabilityAccounts accounts endBalances
      convertedEndNetWorth <-
        mapValidationFailure IncomeStatementErrorConvert $
          convertAccountBalances endPriceGraph currencyTo endNetWorthBalances
      endNetWorthTotal <- case MultiAccount.sum convertedEndNetWorth of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just total -> pure total
      -- Compute begin net worth (assets + liabilities) converted at begin rates
      let beginNetWorthBalances = filterAssetAndLiabilityAccounts accounts beginBalances
      convertedBeginNetWorth <-
        mapValidationFailure IncomeStatementErrorConvert $
          convertAccountBalances beginPriceGraph currencyTo beginNetWorthBalances
      beginNetWorthTotal <- case MultiAccount.sum convertedBeginNetWorth of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just total -> pure total
      -- Net worth change in target currency
      netWorthChange <- case MultiAccount.subtract endNetWorthTotal beginNetWorthTotal of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just diff -> pure diff
      -- Revaluation = -(net worth change) - (income + expenses)
      incomeExpensesSum <- case MultiAccount.add incomeStatementReportTotalIncome incomeStatementReportTotalExpenses of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just total -> pure total
      negatedNetWorthChange <- case MultiAccount.subtract MultiAccount.zero netWorthChange of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just neg -> pure neg
      revaluation <- case MultiAccount.subtract negatedNetWorthChange incomeExpensesSum of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just reval -> pure reval
      if revaluation == MultiAccount.zero
        then pure Nothing
        else pure (Just revaluation)

  -- Net income = income total + expenses total + revaluation
  incomeStatementReportNetIncome <- do
    ieSum <- case MultiAccount.add incomeStatementReportTotalIncome incomeStatementReportTotalExpenses of
      Nothing -> validationFailure IncomeStatementErrorCouldNotSum
      Just total -> pure total
    case incomeStatementReportRevaluation of
      Nothing -> pure ieSum
      Just reval -> case MultiAccount.add ieSum reval of
        Nothing -> validationFailure IncomeStatementErrorCouldNotSum
        Just total -> pure total

  pure IncomeStatementReport {..}

-- | Filter to only asset and liability accounts.
filterAssetAndLiabilityAccounts ::
  M.Map AccountName (GenLocated ann (Account ann)) ->
  AccountBalances ann ->
  AccountBalances ann
filterAssetAndLiabilityAccounts accounts =
  M.filterWithKey (\an _ -> isAssetOrLiability an)
  where
    isAssetOrLiability an = case M.lookup an accounts of
      Nothing -> False
      Just (Located _ acc) -> case accountType acc of
        AccountTypeAssets -> True
        AccountTypeLiabilities -> True
        _ -> False
