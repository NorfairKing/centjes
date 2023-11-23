{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Balance (runCentjesBalance) where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Money.QuantisationFactor
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout
import Text.Printf

runCentjesBalance :: Settings -> BalanceSettings -> IO ()
runCentjesBalance Settings {..} BalanceSettings = runStderrLoggingT $ do
  (declarations, diagnostic) <- loadModules settingLedgerFile
  ledger <- liftIO $ checkValidation diagnostic $ compileDeclarations declarations
  -- TODO: combine these two "checkValidation"s
  accs <- liftIO $ checkValidation diagnostic $ produceBalanceReport ledger
  terminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  let t = table (renderBalanceReport accs)
  liftIO $ putChunksLocaleWith terminalCapabilities $ renderTable t

renderBalanceReport :: BalanceReport ann -> [[Chunk]]
renderBalanceReport = renderBalances . unBalanceReport

renderBalances :: Map AccountName (Money.MultiAccount (Currency ann)) -> [[Chunk]]
renderBalances =
  concatMap
    ( \(an, acc) ->
        case multiAccountChunks acc of
          [] -> []
          (firstChunks : rest) ->
            (accountNameChunk an : firstChunks)
              : map (chunk " " :) rest
    )
    . M.toList

accountNameChunk :: AccountName -> Chunk
accountNameChunk = fore yellow . chunk . unAccountName

multiAccountChunks :: Money.MultiAccount (Currency ann) -> [[Chunk]]
multiAccountChunks ma =
  let accounts = MultiAccount.unMultiAccount ma
   in map
        ( \(c, acc) ->
            let Located _ qf = currencyQuantisationFactor c
             in [ accountChunk qf acc,
                  currencySymbolChunk (currencySymbol c)
                ]
        )
        (M.toList accounts)

currencySymbolChunk :: CurrencySymbol -> Chunk
currencySymbolChunk = fore blue . chunk . unCurrencySymbol

accountChunk :: QuantisationFactor -> Money.Account -> Chunk
accountChunk qf a =
  fore (if a >= Account.zero then green else red)
    . chunk
    . T.pack
    . printf "%10s"
    $ Account.format qf a
