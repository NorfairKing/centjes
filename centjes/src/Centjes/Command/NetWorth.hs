{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.NetWorth
  ( runCentjesNetWorth,
    renderNetWorthReport,
  )
where

import Centjes.Command.Balance (renderBalanceReport)
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
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor (QuantisationFactor)
import System.Exit (die)
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
      OutputFormatCSV -> case report of
        NetWorthReportConverted converted ->
          LB.putStr $ renderNetWorthReportCSV converted
        NetWorthReportMultiCurrency _ ->
          die "CSV output requires --convert to specify a single target currency."

renderNetWorthReport :: NetWorthReport ann -> [Chunk]
renderNetWorthReport = \case
  NetWorthReportConverted converted -> renderConvertedReport converted
  NetWorthReportMultiCurrency multiCurrency -> renderMultiCurrencyReport multiCurrency

renderConvertedReport :: NetWorthReportConvertedData ann -> [Chunk]
renderConvertedReport NetWorthReportConvertedData {..} =
  let Located _ quantisationFactor = currencyQuantisationFactor netWorthReportConvertedCurrency
      symbol = currencySymbol netWorthReportConvertedCurrency
      entriesChunks =
        concatMap
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
          (V.toList netWorthReportConvertedEntries)
      balanceChunks = case netWorthReportConvertedBalanceReport of
        Nothing -> []
        Just balanceReport ->
          chunk "\n" : renderBalanceReport DoNotShowEmpty balanceReport
   in entriesChunks ++ balanceChunks

renderMultiCurrencyReport :: NetWorthReportMultiCurrencyData ann -> [Chunk]
renderMultiCurrencyReport NetWorthReportMultiCurrencyData {..} =
  let width = foldMap (multiAccountMaxWidth . snd) (V.toList netWorthReportMultiCurrencyEntries)
      entriesChunks =
        concatMap
          ( \(day, multiAccount) ->
              let amountRows = multiAccountChunksWithWidth (Just width) multiAccount
                  datePrefix = [fore blue $ chunk $ T.pack $ showDayString day, chunk ": "]
                  indent = [chunk $ T.replicate (T.length ("YYYY-MM-DD" :: T.Text) + 2) " "]
               in case amountRows of
                    [] ->
                      datePrefix ++ [chunk "\n"]
                    (firstRow : restRows) ->
                      datePrefix
                        ++ intersperse (chunk " ") firstRow
                        ++ [chunk "\n"]
                        ++ concatMap (\row -> indent ++ intersperse (chunk " ") row ++ [chunk "\n"]) restRows
          )
          (V.toList netWorthReportMultiCurrencyEntries)
      balanceChunks = case netWorthReportMultiCurrencyBalanceReport of
        Nothing -> []
        Just balanceReport ->
          chunk "\n" : renderBalanceReport DoNotShowEmpty balanceReport
   in entriesChunks ++ balanceChunks

renderNetWorthReportCSV :: NetWorthReportConvertedData ann -> LB.ByteString
renderNetWorthReportCSV NetWorthReportConvertedData {..} =
  let Located _ quantisationFactor = currencyQuantisationFactor netWorthReportConvertedCurrency
      header = "date,balance\n"
      rows = foldMap (renderCSVRow quantisationFactor) (V.toList netWorthReportConvertedEntries)
   in Builder.toLazyByteString (header <> rows)

renderCSVRow :: QuantisationFactor -> (Day, Money.Account) -> Builder.Builder
renderCSVRow quantisationFactor (day, account) =
  Builder.string8 (showDayString day)
    <> Builder.char8 ','
    <> Builder.string8 (Account.format quantisationFactor account)
    <> Builder.char8 '\n'

showDayString :: Day -> String
showDayString = formatTime defaultTimeLocale "%Y-%m-%d"
