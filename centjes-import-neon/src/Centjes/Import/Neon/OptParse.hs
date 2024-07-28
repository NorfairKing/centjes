{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Neon.OptParse where

import qualified Centjes.AccountName as AccountName
import Centjes.Module
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_import_neon (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for neon"

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingInput :: !(Path Abs File),
    settingOutput :: !(Path Abs File),
    settingAssetsAccountName :: !AccountName,
    settingExpensesAccountName :: !AccountName,
    settingIncomeAccountName :: !AccountName
  }

instance HasParser Settings where
  settingsParser =
    subEnv_ "centjes-import-neon" $
      withConfigurableYamlConfig (runIO $ resolveFile' "neon.yaml") $ do
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
              value "neon.cent"
            ]
        settingAssetsAccountName <-
          setting
            [ help "Assets account name",
              reader $ eitherReader AccountName.fromStringOrError,
              name "assets-account",
              value "assets:neon:checking",
              metavar "ACCOUNT_NAME"
            ]
        settingExpensesAccountName <-
          setting
            [ help "Expenses account name",
              reader $ eitherReader AccountName.fromStringOrError,
              name "expenses-account",
              value "expenses:unknown:neon",
              metavar "ACCOUNT_NAME"
            ]
        settingIncomeAccountName <-
          setting
            [ help "Income account name",
              reader $ eitherReader AccountName.fromStringOrError,
              name "income-account",
              value "income:unknown:neon",
              metavar "ACCOUNT_NAME"
            ]
        pure Settings {..}
