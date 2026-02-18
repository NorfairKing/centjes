{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.NetWorth
  ( runCentjesNetWorth,
    renderNetWorthReport,
  )
where

import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.OptParse
import Centjes.Report.NetWorth
import Centjes.Timing
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor (QuantisationFactor)
import Text.Colour

runCentjesNetWorth :: Settings -> NetWorthSettings -> LoggingT IO ()
runCentjesNetWorth Settings {..} NetWorthSettings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- withLoggedDuration "Compile" $ liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    report <-
      withLoggedDuration "Net worth report" $
        liftIO $
          checkValidation diagnostic $
            produceNetWorthReport
              netWorthSettingCurrency
              netWorthSettingBegin
              netWorthSettingEnd
              ledger
    liftIO $ case netWorthSettingOutputFormat of
      OutputFormatTerminal ->
        putChunksLocaleWith settingTerminalCapabilities $
          renderNetWorthReport report
      OutputFormatCSV ->
        LB.putStr $ renderNetWorthReportCSV report

renderNetWorthReport :: NetWorthReport ann -> [Chunk]
renderNetWorthReport NetWorthReport {..} =
  let Located _ quantisationFactor = currencyQuantisationFactor netWorthReportCurrency
      symbol = currencySymbol netWorthReportCurrency
   in concatMap
        ( \(day, account) ->
            let f = fore $ if account >= Account.zero then green else red
             in [ fore blue $ chunk $ T.pack $ showDayString day,
                  chunk ": ",
                  f $ chunk $ T.pack $ Account.format quantisationFactor account,
                  chunk " ",
                  f $ currencySymbolChunk symbol,
                  chunk "\n"
                ]
        )
        (V.toList netWorthReportEntries)

renderNetWorthReportCSV :: NetWorthReport ann -> LB.ByteString
renderNetWorthReportCSV NetWorthReport {..} =
  let Located _ quantisationFactor = currencyQuantisationFactor netWorthReportCurrency
      header = "date,balance\n"
      rows = foldMap (renderCSVRow quantisationFactor) (V.toList netWorthReportEntries)
   in Builder.toLazyByteString (header <> rows)

renderCSVRow :: QuantisationFactor -> (Day, Money.Account) -> Builder.Builder
renderCSVRow quantisationFactor (day, account) =
  Builder.string8 (showDayString day)
    <> Builder.char8 ','
    <> Builder.string8 (Account.format quantisationFactor account)
    <> Builder.char8 '\n'

showDayString :: Day -> String
showDayString = formatTime defaultTimeLocale "%Y-%m-%d"
