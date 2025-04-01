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
import Data.Map.Strict (Map)
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
  terminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  liftIO $ putChunksLocaleWith terminalCapabilities $ renderBalanceReport balanceSettingShowEmpty br

renderBalanceReport :: ShowEmpty -> BalanceReport ann -> [Chunk]
renderBalanceReport se br =
  let t = table (map (map pure) (renderBalanceReportTable se br))
   in renderTable t

renderBalanceReportTable :: ShowEmpty -> BalanceReport ann -> [[Chunk]]
renderBalanceReportTable se br@BalanceReport {..} =
  let width = balanceReportMaxWidth br
   in renderBalances se width balanceReportFilledBalances
        ++ amountLines (fore blue (accountNameChunk "Total")) (multiAccountChunksWithWidth (Just width) balanceReportTotal)

balanceReportMaxWidth :: BalanceReport ann -> Max Word8
balanceReportMaxWidth BalanceReport {..} =
  multiAccountMaxWidth balanceReportTotal
    <> accountBalancesMaxWidth balanceReportBalances
    <> accountBalancesMaxWidth balanceReportFilledBalances

accountBalancesMaxWidth :: AccountBalances ann -> Max Word8
accountBalancesMaxWidth = foldMap multiAccountMaxWidth

renderBalances ::
  ShowEmpty ->
  Max Word8 ->
  Map AccountName (Money.MultiAccount (Currency ann)) ->
  [[Chunk]]
renderBalances se width =
  concatMap
    (\(an, acc) -> amountLines (accountNameChunk an) $ multiAccountChunksWithWidth (Just width) acc)
    . ( case se of
          ShowEmpty -> id
          DoNotShowEmpty -> filter (not . null . MultiAccount.unMultiAccount . snd) -- Don't show account with empty balances
      )
    . M.toList

amountLines :: Chunk -> [[Chunk]] -> [[Chunk]]
amountLines header cs = hCatTable [[[header]], cs]
