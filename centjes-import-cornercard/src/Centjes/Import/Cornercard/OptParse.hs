{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Cornercard.OptParse where

import qualified Centjes.AccountName as AccountName
import Centjes.Module
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_import_cornercard (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for cornercard"

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingInput :: !(Path Abs File),
    settingOutput :: !(Path Abs File),
    settingLiabilitiesAccountName :: !AccountName,
    settingExpensesAccountName :: !AccountName,
    settingIncomeAccountName :: !AccountName
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-import-cornercard" $
    withConfigurableYamlConfig (runIO $ resolveFile' "cornercard.yaml") $ do
      settingLedgerFile <-
        filePathSetting
          [ help "main ledger file",
            short 'l',
            name "ledger",
            value "ledger.cent"
          ]
      settingInput <-
        filePathSetting
          [ help "input file",
            argument,
            metavar "CSV_FILE"
          ]
      settingOutput <-
        filePathSetting
          [ help "help output ledger file",
            short 'o',
            name "output",
            value "cornercard.cent"
          ]
      settingLiabilitiesAccountName <-
        setting
          [ help "Liabilities account name",
            reader $ eitherReader AccountName.fromStringOrError,
            name "liabilities-account",
            value "liabilities:cornercard",
            metavar "ACCOUNT_NAME"
          ]
      settingExpensesAccountName <-
        setting
          [ help "Expenses account name",
            reader $ eitherReader AccountName.fromStringOrError,
            name "expenses-account",
            value "expenses:unknown:cornercard",
            metavar "ACCOUNT_NAME"
          ]
      settingIncomeAccountName <-
        setting
          [ help "Income account name",
            reader $ eitherReader AccountName.fromStringOrError,
            name "income-account",
            value "income:unknown:cornercard",
            metavar "ACCOUNT_NAME"
          ]
      pure Settings {..}
