module Centjes.Chart (runCentjesChart) where

import Centjes.Chart.Command
import Centjes.Chart.OptParse
import Control.Monad.Logger

runCentjesChart :: IO ()
runCentjesChart = do
  Instructions d settings <- getInstructions
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingsLogLevel settings) $
      case d of
        DispatchAll allSettings -> runCentjesChartAll settings allSettings
