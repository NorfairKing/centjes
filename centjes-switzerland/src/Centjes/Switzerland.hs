module Centjes.Switzerland (runCentjesSwitzerland) where

import Centjes.Switzerland.Command
import Centjes.Switzerland.OptParse

runCentjesSwitzerland :: IO ()
runCentjesSwitzerland = do
  Instructions d settings <- getInstructions
  case d of
    DispatchTaxes taxesSettings -> runCentjesSwitzerlandTaxes settings taxesSettings
    DispatchVAT taxesSettings -> runCentjesSwitzerlandVAT settings taxesSettings
    DispatchDownloadRates downloadRatesSettings -> runCentjesSwitzerlandDownloadRates settings downloadRatesSettings
