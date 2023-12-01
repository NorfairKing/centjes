module Centjes (runCentjes) where

import Centjes.Command
import Centjes.OptParse

runCentjes :: IO ()
runCentjes = do
  Instructions d settings <- getInstructions
  case d of
    DispatchCheck cs -> runCentjesCheck settings cs
    DispatchRegister cs -> runCentjesRegister settings cs
    DispatchBalance cs -> runCentjesBalance settings cs
    DispatchFormat cs -> runCentjesFormat settings cs
