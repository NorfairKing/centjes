{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.IncomeStatement
  ( runCentjesIncomeStatement,
    renderIncomeStatementReport,
  )
where

import Centjes.Command.Balance
  ( accountBalancesMaxWidth,
    blankChunks,
    buildAccountForest,
    flattenForestWithContext,
    renderTreeAccountRow,
    totalLines,
  )
import Centjes.Compile
import Centjes.Formatting
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.EvaluatedLedger
import Centjes.Report.IncomeStatement
import Centjes.Timing
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Semigroup
import Data.Word
import qualified Money.Account as Account
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Text.Colour
import Text.Colour.Layout

runCentjesIncomeStatement :: Settings -> IncomeStatementSettings -> LoggingT IO ()
runCentjesIncomeStatement Settings {..} IncomeStatementSettings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- withLoggedDuration "Compile" $ liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    evaluatedLedger <-
      withLoggedDuration "Evaluated ledger" $
        liftIO $
          checkValidation diagnostic $
            produceEvaluatedLedger ledger
    report <-
      withLoggedDuration "Income statement report" $
        liftIO $
          checkValidation diagnostic $
            produceIncomeStatementReport
              incomeStatementSettingFilter
              incomeStatementSettingBegin
              incomeStatementSettingEnd
              incomeStatementSettingCurrency
              incomeStatementSettingShowVirtual
              evaluatedLedger
    liftIO $ putChunksLocaleWith settingTerminalCapabilities $ renderIncomeStatementReport incomeStatementSettingShowEmpty report

renderIncomeStatementReport :: ShowEmpty -> IncomeStatementReport ann -> [Chunk]
renderIncomeStatementReport se report =
  let allRows = renderIncomeStatementRows se report
      t = table (map (map pure) allRows)
   in renderTable t

-- | Negate a MultiAccount for display purposes.
-- In the income statement, we flip signs so that income is positive and expenses are negative.
negateMultiAccount :: Money.MultiAccount c -> Money.MultiAccount c
negateMultiAccount = MultiAccount.MultiAccount . M.map Account.negate . MultiAccount.unMultiAccount

-- | Negate all balances in an AccountBalances map for display purposes.
negateAccountBalances :: AccountBalances ann -> AccountBalances ann
negateAccountBalances = M.map negateMultiAccount

renderIncomeStatementRows :: ShowEmpty -> IncomeStatementReport ann -> [[Chunk]]
renderIncomeStatementRows se report@IncomeStatementReport {..} =
  let width = incomeStatementMaxWidth se report
      headerRow = hCatTable [[[chunk ""]], [[fore white $ chunk "Balance", chunk ""]], [[fore white $ chunk "Total", chunk ""]]]
      -- Income section (negated: income becomes positive)
      negatedIncome = negateAccountBalances incomeStatementReportIncome
      negatedFilledIncome = negateAccountBalances incomeStatementReportFilledIncome
      negatedTotalIncome = negateMultiAccount incomeStatementReportTotalIncome
      incomeAccountsToShow = case se of
        ShowEmpty -> negatedFilledIncome
        DoNotShowEmpty -> M.filter (not . null . MultiAccount.unMultiAccount) negatedFilledIncome
      incomeForest = buildAccountForest incomeAccountsToShow
      incomeFlattenedWithContext = flattenForestWithContext incomeForest
      incomeRows =
        concatMap (renderTreeAccountRow width negatedIncome) incomeFlattenedWithContext
          ++ totalLines width negatedTotalIncome
      -- Expenses section (negated: expenses become negative)
      negatedExpenses = negateAccountBalances incomeStatementReportExpenses
      negatedFilledExpenses = negateAccountBalances incomeStatementReportFilledExpenses
      negatedTotalExpenses = negateMultiAccount incomeStatementReportTotalExpenses
      expensesAccountsToShow = case se of
        ShowEmpty -> negatedFilledExpenses
        DoNotShowEmpty -> M.filter (not . null . MultiAccount.unMultiAccount) negatedFilledExpenses
      expensesForest = buildAccountForest expensesAccountsToShow
      expensesFlattenedWithContext = flattenForestWithContext expensesForest
      expensesRows =
        concatMap (renderTreeAccountRow width negatedExpenses) expensesFlattenedWithContext
          ++ totalLines width negatedTotalExpenses
      -- Revaluation section (negated: losses become negative)
      revaluationRows = case incomeStatementReportRevaluation of
        Nothing -> []
        Just reval ->
          let negatedReval = negateMultiAccount reval
              revalChunks = multiAccountChunksWithWidth (Just width) negatedReval
           in [] : hCatTable [[[fore blue $ chunk "Revaluation"]], blankChunks revalChunks, revalChunks]
      -- Net income section (negated: profit becomes positive)
      negatedNetIncome = negateMultiAccount incomeStatementReportNetIncome
      netIncomeChunks = multiAccountChunksWithWidth (Just width) negatedNetIncome
      netIncomeRows = hCatTable [[[fore blue $ chunk "Net Income"]], blankChunks netIncomeChunks, netIncomeChunks]
   in headerRow
        ++ incomeRows
        ++ [[]] -- blank row separator
        ++ headerRow
        ++ expensesRows
        ++ revaluationRows
        ++ [[]] -- blank row separator
        ++ netIncomeRows

-- | Compute the maximum width across all sections of the income statement
-- so that all columns align consistently.
incomeStatementMaxWidth :: ShowEmpty -> IncomeStatementReport ann -> Max Word8
incomeStatementMaxWidth se IncomeStatementReport {..} =
  -- Income section widths
  multiAccountMaxWidth incomeStatementReportTotalIncome
    <> accountBalancesMaxWidth incomeStatementReportIncome
    <> accountBalancesMaxWidth incomeStatementReportFilledIncome
    -- Expenses section widths
    <> multiAccountMaxWidth incomeStatementReportTotalExpenses
    <> accountBalancesMaxWidth incomeStatementReportExpenses
    <> accountBalancesMaxWidth incomeStatementReportFilledExpenses
    -- Net income width
    <> multiAccountMaxWidth incomeStatementReportNetIncome
    -- Revaluation width
    <> maybe mempty multiAccountMaxWidth incomeStatementReportRevaluation
    -- Account widths from the shown accounts
    <> accountBalancesMaxWidth incomeAccountsToShow
    <> accountBalancesMaxWidth expensesAccountsToShow
  where
    incomeAccountsToShow = case se of
      ShowEmpty -> incomeStatementReportFilledIncome
      DoNotShowEmpty -> M.filter (not . null . MultiAccount.unMultiAccount) incomeStatementReportFilledIncome
    expensesAccountsToShow = case se of
      ShowEmpty -> incomeStatementReportFilledExpenses
      DoNotShowEmpty -> M.filter (not . null . MultiAccount.unMultiAccount) incomeStatementReportFilledExpenses
