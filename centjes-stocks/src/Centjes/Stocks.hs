{-# LANGUAGE RecordWildCards #-}

module Centjes.Stocks (runCentjesStocks) where

import Centjes.Stocks.Command.DownloadRates
import Centjes.Stocks.OptParse

runCentjesStocks :: IO ()
runCentjesStocks = do
  settings@Settings {..} <- getSettings
  case settingsCommand of
    CommandDownloadRates downloadRatesSettings ->
      runCentjesStocksDownloadRates settings downloadRatesSettings
