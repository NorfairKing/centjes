module Centjes.Chart (runCentjesChart) where

import Centjes.Chart.Command
import Centjes.Chart.OptParse

runCentjesChart :: IO ()
runCentjesChart = do
  Instructions d settings <- getInstructions
  case d of
    DispatchAll -> runCentjesChartAll settings
