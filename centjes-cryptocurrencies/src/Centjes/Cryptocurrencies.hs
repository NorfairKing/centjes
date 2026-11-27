{-# LANGUAGE RecordWildCards #-}

module Centjes.Cryptocurrencies (runCentjesCryptocurrencies) where

import Centjes.Cryptocurrencies.Command.DownloadRates
import Centjes.Cryptocurrencies.OptParse

runCentjesCryptocurrencies :: IO ()
runCentjesCryptocurrencies = do
  settings@Settings {..} <- getSettings
  case settingsCommand of
    CommandDownloadRates downloadRatesSettings ->
      runCentjesCryptocurrenciesDownloadRates settings downloadRatesSettings
