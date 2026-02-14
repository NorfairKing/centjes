{-# LANGUAGE RecordWildCards #-}

module Centjes.Cryptocurrencies (runCentjesCryptocurrencies) where

import Centjes.Cryptocurrencies.Command.DownloadRates
import Centjes.Cryptocurrencies.OptParse

runCentjesCryptocurrencies :: IO ()
runCentjesCryptocurrencies = do
  settings@Settings {..} <- getSettings
  case settingCommand of
    CommandDownloadRates downloadRatesSettings ->
      runCentjesCryptocurrenciesDownloadRates settings downloadRatesSettings
