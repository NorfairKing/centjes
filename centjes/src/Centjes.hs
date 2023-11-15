module Centjes
  ( runCentjes,
  )
where

import Centjes.Command
import Centjes.OptParse

runCentjes :: IO ()
runCentjes = do
  Instructions d Settings <- getInstructions
  case d of
    DispatchBalance cs -> runCentjesBalance cs
    DispatchFormat cs -> runCentjesFormat cs
