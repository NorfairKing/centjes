module Centjes (runCentjes) where

import Centjes.Command
import Centjes.OptParse
import Control.Monad.Logger
import qualified Data.Text as T
import Text.Show.Pretty

runCentjes :: IO ()
runCentjes = do
  instructions@(Instructions d settings) <- getInstructions
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= settingLogLevel settings) $ do
      logDebugN $ T.pack $ ppShow instructions
      case d of
        DispatchCheck cs -> runCentjesCheck settings cs
        DispatchRegister cs -> runCentjesRegister settings cs
        DispatchBalance cs -> runCentjesBalance settings cs
        DispatchFormat cs -> runCentjesFormat settings cs
        DispatchRatesGraph cs -> runCentjesRatesGraph settings cs
