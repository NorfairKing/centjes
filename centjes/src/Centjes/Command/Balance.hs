{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Balance
  ( runCentjesBalance,
    renderBalanceReport,
    AccountTree (..),
    TreeRenderContext (..),
    buildAccountForest,
    flattenForestWithContext,
    renderTreeAccountRow,
    accountBalancesMaxWidth,
    blankChunks,
    totalLines,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Report.EvaluatedLedger
import Centjes.Timing
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Semigroup
import Data.Text (Text)
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
    evaluatedLedger <-
      withLoggedDuration "Evaluated ledger" $
        liftIO $
          checkValidation diagnostic $
            produceEvaluatedLedger ledger
    br <-
      withLoggedDuration "Balance report" $
        liftIO $
          checkValidation diagnostic $
            produceBalanceReportFromEvaluatedLedger
              balanceSettingFilter
              balanceSettingEnd
              balanceSettingCurrency
              balanceSettingShowVirtual
              evaluatedLedger
    liftIO $ putChunksLocaleWith settingTerminalCapabilities $ renderBalanceReport balanceSettingShowEmpty br

renderBalanceReport :: ShowEmpty -> BalanceReport ann -> [Chunk]
renderBalanceReport se br =
  let t = table (map (map pure) (renderBalanceReportTable se br))
   in renderTable t

renderBalanceReportTable :: ShowEmpty -> BalanceReport ann -> [[Chunk]]
renderBalanceReportTable se br@BalanceReport {..} =
  let width = balanceReportMaxWidth br
      -- Filter accounts BEFORE building tree so isLast is computed correctly
      accountsToShow = case se of
        ShowEmpty -> balanceReportFilledBalances
        DoNotShowEmpty -> M.filter (not . null . MultiAccount.unMultiAccount) balanceReportFilledBalances
      forest = buildAccountForest accountsToShow
      flattenedWithContext = flattenForestWithContext forest
      -- Header row: name column (includes tree), balance column (2 cells), total column (2 cells)
      headerRow = hCatTable [[[chunk ""]], [[fore white $ chunk "Balance", chunk ""]], [[fore white $ chunk "Total", chunk ""]]]
   in headerRow
        ++ concatMap (renderTreeAccountRow width balanceReportBalances) flattenedWithContext
        ++ totalLines width balanceReportTotal

-- | A tree node representing an account and its children
data AccountTree ann = AccountTree
  { accountTreeName :: !Text, -- Leaf component only (e.g., "bar" for "assets:foo:bar")
    accountTreeFullName :: !AccountName, -- Full name for balance lookups
    accountTreeFilledBalance :: !(Money.MultiAccount (Currency ann)),
    accountTreeChildren :: ![AccountTree ann]
  }

-- | Build a forest of account trees from the account balances
buildAccountForest :: AccountBalances ann -> [AccountTree ann]
buildAccountForest balances =
  let accounts = sortOn fst $ M.toList balances
      -- Find root accounts (those whose parent is not in the map)
      isRoot an = case AccountName.parent an of
        Nothing -> True
        Just p -> not $ M.member p balances
      roots = filter (isRoot . fst) accounts
   in map (buildTree balances accounts) roots
  where
    buildTree :: AccountBalances ann -> [(AccountName, Money.MultiAccount (Currency ann))] -> (AccountName, Money.MultiAccount (Currency ann)) -> AccountTree ann
    buildTree bs allAccounts (an, bal) =
      let -- Find direct children: accounts whose parent is this account
          children = filter (\(childAn, _) -> AccountName.parent childAn == Just an) allAccounts
          leafName = NE.head (AccountName.unAccountName an) -- First element is the leaf (stored in reverse)
       in AccountTree
            { accountTreeName = leafName,
              accountTreeFullName = an,
              accountTreeFilledBalance = bal,
              accountTreeChildren = map (buildTree bs allAccounts) children
            }

-- | Context for rendering tree prefixes
data TreeRenderContext = TreeRenderContext
  { treeRenderContextPrefixStack :: ![Bool], -- Stack of "is last child" for ancestors (most recent first), excluding root
    treeRenderContextIsLast :: !Bool, -- Is this node last among siblings?
    treeRenderContextIsRoot :: !Bool -- Is this a root node?
  }

-- | Flatten a forest into a list with tree render context for each node
flattenForestWithContext :: [AccountTree ann] -> [(TreeRenderContext, AccountTree ann)]
flattenForestWithContext = concatMap flattenRootWithContext
  where
    markLast :: [a] -> [(Bool, a)]
    markLast [] = []
    markLast [x] = [(True, x)]
    markLast (x : xs) = (False, x) : markLast xs

    -- Root nodes don't get tree prefixes and don't contribute to prefix stack
    flattenRootWithContext :: AccountTree ann -> [(TreeRenderContext, AccountTree ann)]
    flattenRootWithContext tree =
      let ctx = TreeRenderContext {treeRenderContextPrefixStack = [], treeRenderContextIsLast = True, treeRenderContextIsRoot = True}
          childResults = concatMap (flattenWithContext []) (markLast (accountTreeChildren tree))
       in (ctx, tree) : childResults

    flattenWithContext :: [Bool] -> (Bool, AccountTree ann) -> [(TreeRenderContext, AccountTree ann)]
    flattenWithContext prefixStack (isLast, tree) =
      let ctx = TreeRenderContext {treeRenderContextPrefixStack = prefixStack, treeRenderContextIsLast = isLast, treeRenderContextIsRoot = False}
          childPrefixStack = isLast : prefixStack
          childResults = concatMap (flattenWithContext childPrefixStack) (markLast (accountTreeChildren tree))
       in (ctx, tree) : childResults

balanceReportMaxWidth :: BalanceReport ann -> Max Word8
balanceReportMaxWidth BalanceReport {..} =
  multiAccountMaxWidth balanceReportTotal
    <> accountBalancesMaxWidth balanceReportBalances
    <> accountBalancesMaxWidth balanceReportFilledBalances

accountBalancesMaxWidth :: AccountBalances ann -> Max Word8
accountBalancesMaxWidth = foldMap multiAccountMaxWidth

-- | Render a tree account row with tree prefix and leaf name
renderTreeAccountRow ::
  Max Word8 ->
  AccountBalances ann ->
  (TreeRenderContext, AccountTree ann) ->
  [[Chunk]]
renderTreeAccountRow width actualBalances (ctx, AccountTree {..}) =
  let mActualAmount = M.lookup accountTreeFullName actualBalances
      filledChunks = multiAccountChunksWithWidth (Just width) accountTreeFilledBalance
      -- Show blank if no direct transactions, otherwise show the actual balance
      actualChunks = case mActualAmount of
        Nothing -> blankChunks filledChunks
        Just amt -> multiAccountChunksWithWidth (Just width) amt
      -- Name column: prefix + connector + name all in one single chunk
      nameCol = [[fore white $ chunk $ renderTreePrefixText ctx <> accountTreeName]]
   in hCatTable [nameCol, actualChunks, filledChunks]

-- | Render tree prefix with connector as text using Unicode box-drawing characters
renderTreePrefixText :: TreeRenderContext -> Text
renderTreePrefixText TreeRenderContext {..}
  | treeRenderContextIsRoot = "" -- Root level, no prefix
  | otherwise =
      let ancestorPrefixes = map ancestorPrefix (reverse treeRenderContextPrefixStack)
          connector =
            if treeRenderContextIsLast
              then "└─ "
              else "├─ "
       in mconcat ancestorPrefixes <> connector
  where
    ancestorPrefix :: Bool -> Text
    ancestorPrefix isLast
      | isLast = "   " -- Ancestor was last child, no continuation line (3 spaces to match connector width)
      | otherwise = "│  " -- Ancestor has siblings below, draw continuation line

-- | Create blank placeholder chunks matching the structure of the given chunks
blankChunks :: [[Chunk]] -> [[Chunk]]
blankChunks cs = replicate (length cs) [chunk "", chunk ""]

totalLines :: Max Word8 -> Money.MultiAccount (Currency ann) -> [[Chunk]]
totalLines width total =
  let totalChunks = multiAccountChunksWithWidth (Just width) total
   in hCatTable [[[fore blue $ chunk "Total"]], blankChunks totalChunks, totalChunks]
