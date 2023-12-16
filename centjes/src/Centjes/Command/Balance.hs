{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Balance (runCentjesBalance) where

import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Semigroup
import Data.Word
import qualified Money.MultiAccount as Money (MultiAccount)
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout

runCentjesBalance :: Settings -> BalanceSettings -> IO ()
runCentjesBalance Settings {..} BalanceSettings {..} = runStderrLoggingT $ do
  (declarations, diagnostic) <- loadModules settingLedgerFile
  ledger <- liftIO $ checkValidation diagnostic $ compileDeclarations declarations
  accs <-
    liftIO $
      checkValidation diagnostic $
        produceBalanceReport balanceSettingCurrency ledger
  terminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  let t = table (renderBalanceReport accs)
  liftIO $ putChunksLocaleWith terminalCapabilities $ renderTable t

renderBalanceReport :: BalanceReport ann -> [[Chunk]]
renderBalanceReport br@(BalanceReport m) =
  let width = balanceReportMaxWidth br
   in renderBalances width m

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
    ( \(an, acc) ->
        case multiAccountChunksWithWidth (Just width) acc of
          [] -> []
          (firstChunks : rest) ->
            (accountNameChunk an : firstChunks)
              : map (chunk " " :) rest
    )
    . M.toList
