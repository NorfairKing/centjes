{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Neon.OptParse where

import qualified Centjes.AccountName as AccountName
import Centjes.Module
import Centjes.Validation (renderInvalid)
import Data.Bifunctor (first)
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_import_neon (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for neon"

data Settings = Settings
  { settingsLedgerFile :: !(Path Abs File),
    settingsInput :: !(Path Abs File),
    settingsOutput :: !(Path Abs File),
    settingsAssetsAccountName :: !AccountName,
    settingsExpensesAccountName :: !AccountName,
    settingsIncomeAccountName :: !AccountName
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-import-neon" $
    withConfigurableYamlConfig (runIO $ resolveFile' "neon.yaml") $ do
      settingsLedgerFile <-
        filePathSetting
          [ help "main ledger file",
            short 'l',
            name "ledger",
            value "ledger.cent"
          ]
      settingsInput <-
        filePathSetting
          [ help "input file",
            argument,
            metavar "CSV_FILE"
          ]
      settingsOutput <-
        filePathSetting
          [ help "help output ledger file",
            short 'o',
            name "output",
            value "neon.cent"
          ]
      settingsAssetsAccountName <-
        setting
          [ help "Assets account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "assets-account",
            value "assets:neon:checking",
            metavar "ACCOUNT_NAME"
          ]
      settingsExpensesAccountName <-
        setting
          [ help "Expenses account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "expenses-account",
            value "expenses:unknown:neon",
            metavar "ACCOUNT_NAME"
          ]
      settingsIncomeAccountName <-
        setting
          [ help "Income account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "income-account",
            value "income:unknown:neon",
            metavar "ACCOUNT_NAME"
          ]
      pure Settings {..}
