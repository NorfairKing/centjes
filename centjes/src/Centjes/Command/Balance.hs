{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Balance
  ( runCentjesBalance,
    renderBalanceReportTable,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Foldable
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Semigroup
import Data.Word
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout

runCentjesBalance :: Settings -> BalanceSettings -> IO ()
runCentjesBalance Settings {..} BalanceSettings {..} = runStderrLoggingT $ do
  (declarations, diagnostic) <- loadModules settingLedgerFile
  ledger <- liftIO $ checkValidation diagnostic $ compileDeclarations declarations
  br <-
    liftIO $
      checkValidation diagnostic $
        produceBalanceReport balanceSettingFilter balanceSettingCurrency ledger
  terminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  liftIO $ putChunksLocaleWith terminalCapabilities $ renderBalanceReportTable $ fillBalanceReport br

-- Should this be in Centjes.Report.Balance?
fillBalanceReport :: forall ann. Ord ann => BalanceReport ann -> BalanceReport ann
fillBalanceReport = BalanceReport . go . unBalanceReport
  where
    go :: AccountBalances ann -> AccountBalances ann
    go as = foldl' go' as (M.toList as)
    go' :: AccountBalances ann -> (AccountName, Money.MultiAccount (Currency ann)) -> AccountBalances ann
    go' as (an, am) = foldl' (go'' am) as (AccountName.ancestors an)
    go'' ::
      Money.MultiAccount (Currency ann) ->
      AccountBalances ann ->
      AccountName ->
      AccountBalances ann
    go'' am as an = case M.lookup an as of
      Nothing -> M.insert an am as
      Just am' -> case MultiAccount.add am am' of
        Nothing -> as -- TODO error somehow
        Just am'' -> M.insert an am'' as

renderBalanceReportTable :: Ord ann => BalanceReport ann -> [Chunk]
renderBalanceReportTable br =
  let t = table (renderBalanceReport br)
   in renderTable t

renderBalanceReport :: Ord ann => BalanceReport ann -> [[Chunk]]
renderBalanceReport br@(BalanceReport m) =
  let width = balanceReportMaxWidth br
      totalAmount = fromMaybe (error "Error somehow?") (MultiAccount.sum m)
   in renderBalances width m
        ++ amountLines (fore blue (accountNameChunk "Total")) (multiAccountChunksWithWidth (Just width) totalAmount)

balanceReportMaxWidth :: BalanceReport ann -> Max Word8
balanceReportMaxWidth = accountBalancesMaxWidth . unBalanceReport

accountBalancesMaxWidth :: AccountBalances ann -> Max Word8
accountBalancesMaxWidth = foldMap multiAccountMaxWidth

renderBalances ::
  Max Word8 ->
  Map AccountName (Money.MultiAccount (Currency ann)) ->
  [[Chunk]]
renderBalances width =
  concatMap
    (\(an, acc) -> amountLines (accountNameChunk an) $ multiAccountChunksWithWidth (Just width) acc)
    . M.toList

-- TODO use the hCatTable
amountLines :: Chunk -> [[Chunk]] -> [[Chunk]]
amountLines header cs = case cs of
  [] -> [[header]]
  (firstChunks : rest) -> (header : firstChunks) : map (chunk " " :) rest
