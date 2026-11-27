{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Revolut.OptParse where

import qualified Centjes.AccountName as AccountName
import Centjes.Module
import Centjes.Validation (renderInvalid)
import Data.Bifunctor (first)
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_import_revolut (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for revolut"

data Settings = Settings
  { settingsLedgerFile :: !(Path Abs File),
    settingsInput :: !(Path Abs File),
    settingsOutput :: !(Path Abs File),
    settingsAssetsAccountName :: !AccountName,
    settingsExpensesAccountName :: !AccountName,
    settingsIncomeAccountName :: !AccountName,
    settingsFeesAccountName :: !AccountName
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-import-revolut" $
    withConfigurableYamlConfig (runIO $ resolveFile' "revolut.yaml") $ do
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
            value "revolut.cent"
          ]
      settingsAssetsAccountName <-
        setting
          [ help "Assets account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "assets-account",
            value "assets:revolut:checking",
            metavar "ACCOUNT_NAME"
          ]
      settingsExpensesAccountName <-
        setting
          [ help "Expenses account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "expenses-account",
            value "expenses:unknown:revolut",
            metavar "ACCOUNT_NAME"
          ]
      settingsIncomeAccountName <-
        setting
          [ help "Income account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "income-account",
            value "income:unknown:revolut",
            metavar "ACCOUNT_NAME"
          ]
      settingsFeesAccountName <-
        setting
          [ help "Fees account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "fees-account",
            value "expenses:banking:revolut",
            metavar "ACCOUNT_NAME"
          ]
      pure Settings {..}
