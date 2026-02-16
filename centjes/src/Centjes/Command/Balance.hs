{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Balance
  ( runCentjesBalance,
    renderBalanceReport,
  )
where

import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Timing
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Semigroup
import Data.Word
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Text.Colour
import Text.Colour.Layout

runCentjesBalance :: Settings -> BalanceSettings -> LoggingT IO ()
runCentjesBalance Settings {..} BalanceSettings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- withLoggedDuration "Compile" $ liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    br <-
      withLoggedDuration "Balance report" $
        liftIO $
          checkValidation diagnostic $
            produceBalanceReport
              balanceSettingFilter
              balanceSettingEnd
              balanceSettingCurrency
              balanceSettingShowVirtual
              ledger
    liftIO $ putChunksLocaleWith settingTerminalCapabilities $ renderBalanceReport balanceSettingShowEmpty br

renderBalanceReport :: ShowEmpty -> BalanceReport ann -> [Chunk]
renderBalanceReport se br =
  let t = table (map (map pure) (renderBalanceReportTable se br))
   in renderTable t

renderBalanceReportTable :: ShowEmpty -> BalanceReport ann -> [[Chunk]]
renderBalanceReportTable se br@BalanceReport {..} =
  let width = balanceReportMaxWidth br
      accounts = M.toList balanceReportFilledBalances
      filtered = case se of
        ShowEmpty -> accounts
        DoNotShowEmpty -> filter (not . null . MultiAccount.unMultiAccount . snd) accounts
      -- Header row: each column needs two cells (amount position + currency position) to align with data rows
      headerRow = hCatTable [[[chunk ""]], [[fore white $ chunk "Balance", chunk ""]], [[fore white $ chunk "Total", chunk ""]]]
   in headerRow
        ++ concatMap (renderAccountRow width balanceReportBalances) filtered
        ++ totalLines width balanceReportTotal

balanceReportMaxWidth :: BalanceReport ann -> Max Word8
balanceReportMaxWidth BalanceReport {..} =
  multiAccountMaxWidth balanceReportTotal
    <> accountBalancesMaxWidth balanceReportBalances
    <> accountBalancesMaxWidth balanceReportFilledBalances

accountBalancesMaxWidth :: AccountBalances ann -> Max Word8
accountBalancesMaxWidth = foldMap multiAccountMaxWidth

renderAccountRow ::
  Max Word8 ->
  AccountBalances ann ->
  (AccountName, Money.MultiAccount (Currency ann)) ->
  [[Chunk]]
renderAccountRow width actualBalances (an, filledAmount) =
  let mActualAmount = M.lookup an actualBalances
      filledChunks = multiAccountChunksWithWidth (Just width) filledAmount
      -- Show blank if no direct transactions, otherwise show the actual balance
      -- We need placeholder chunks to maintain column alignment
      actualChunks = case mActualAmount of
        Nothing -> blankChunks filledChunks
        Just amt -> multiAccountChunksWithWidth (Just width) amt
   in hCatTable [[[accountNameChunk an]], actualChunks, filledChunks]

-- | Create blank placeholder chunks matching the structure of the given chunks
blankChunks :: [[Chunk]] -> [[Chunk]]
blankChunks cs = replicate (length cs) [chunk "", chunk ""]

totalLines :: Max Word8 -> Money.MultiAccount (Currency ann) -> [[Chunk]]
totalLines width total =
  let totalChunks = multiAccountChunksWithWidth (Just width) total
   in hCatTable [[[fore blue $ accountNameChunk "Total"]], blankChunks totalChunks, totalChunks]
