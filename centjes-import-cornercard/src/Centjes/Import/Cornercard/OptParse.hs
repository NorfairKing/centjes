{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Cornercard.OptParse where

import qualified Centjes.AccountName as AccountName
import Centjes.Module
import Centjes.Validation (renderInvalid)
import Data.Bifunctor (first)
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_import_cornercard (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for cornercard"

data Settings = Settings
  { settingsLedgerFile :: !(Path Abs File),
    settingsInput :: !(Path Abs File),
    settingsOutput :: !(Path Abs File),
    settingsLiabilitiesAccountName :: !AccountName,
    settingsExpensesAccountName :: !AccountName,
    settingsIncomeAccountName :: !AccountName
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-import-cornercard" $
    withConfigurableYamlConfig (runIO $ resolveFile' "cornercard.yaml") $ do
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
            value "cornercard.cent"
          ]
      settingsLiabilitiesAccountName <-
        setting
          [ help "Liabilities account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "liabilities-account",
            value "liabilities:cornercard",
            metavar "ACCOUNT_NAME"
          ]
      settingsExpensesAccountName <-
        setting
          [ help "Expenses account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "expenses-account",
            value "expenses:unknown:cornercard",
            metavar "ACCOUNT_NAME"
          ]
      settingsIncomeAccountName <-
        setting
          [ help "Income account name",
            reader $ eitherReader (first renderInvalid . AccountName.fromStringOrError),
            name "income-account",
            value "income:unknown:cornercard",
            metavar "ACCOUNT_NAME"
          ]
      pure Settings {..}
