{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Wise (runCentjesImportWise) where

import Centjes.Import.Wise.Command.Fetch
import Centjes.Import.Wise.Command.Import
import Centjes.Import.Wise.OptParse

runCentjesImportWise :: IO ()
runCentjesImportWise = do
  Settings {..} <- getSettings
  case settingCommand of
    CommandFetch fetchSettings -> runCentjesImportWiseFetch fetchSettings
    CommandImport importSettings -> runCentjesImportWiseImport settingLedgerFile importSettings
