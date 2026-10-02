{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Wise.OptParse
  ( getSettings,
    Settings (..),
    declarationSettings,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Import.Wise.Declarations (DeclarationSettings (..))
import Centjes.Module
import Data.List.NonEmpty (NonEmpty)
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_import_wise (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for wise"

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    -- | The statements to read.
    --
    -- More than one, because a conversion is written into the statement of both
    -- balances it moved between, and only reading both makes it one transaction
    -- rather than two halves.
    settingInputs :: !(NonEmpty (Path Abs File)),
    settingOutput :: !(Path Abs File),
    settingAssetsAccountName :: !AccountName,
    settingExpensesAccountName :: !AccountName,
    settingIncomeAccountName :: !AccountName,
    settingFeesAccountName :: !AccountName,
    settingFeeTags :: ![Tag]
  }

instance HasParser Settings where
  settingsParser = parseSettings

-- | The part of the settings the emitter needs.
declarationSettings :: Settings -> DeclarationSettings
declarationSettings Settings {..} =
  DeclarationSettings
    { declarationSettingAssetsAccountName = settingAssetsAccountName,
      declarationSettingExpensesAccountName = settingExpensesAccountName,
      declarationSettingIncomeAccountName = settingIncomeAccountName,
      declarationSettingFeesAccountName = settingFeesAccountName,
      declarationSettingFeeTags = settingFeeTags
    }

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-import-wise" $
    withConfigurableYamlConfig (runIO $ resolveFile' "wise.yaml") $ do
      settingLedgerFile <-
        filePathSetting
          [ help "main ledger file",
            short 'l',
            name "ledger",
            value "ledger.cent",
            metavar "FILE_PATH"
          ]
      settingInputs <-
        someNonEmpty $
          filePathSetting
            [ help "statement to import, one per balance",
              argument,
              metavar "CSV_FILE"
            ]
      settingOutput <-
        filePathSetting
          [ help "output ledger file, which this importer rewrites",
            short 'o',
            name "output",
            value "wise-import.cent",
            metavar "FILE_PATH"
          ]
      settingAssetsAccountName <-
        setting
          [ help "Assets account name, which holds every currency",
            reader $ eitherReader AccountName.fromStringOrError,
            name "assets-account",
            value "assets:wise",
            metavar "ACCOUNT_NAME"
          ]
      settingExpensesAccountName <-
        setting
          [ help "Expenses account name",
            reader $ eitherReader AccountName.fromStringOrError,
            name "expenses-account",
            value "expenses:unknown:wise",
            metavar "ACCOUNT_NAME"
          ]
      settingIncomeAccountName <-
        setting
          [ help "Income account name",
            reader $ eitherReader AccountName.fromStringOrError,
            name "income-account",
            value "income:unknown:wise",
            metavar "ACCOUNT_NAME"
          ]
      settingFeesAccountName <-
        setting
          [ help "Fees account name",
            reader $ eitherReader AccountName.fromStringOrError,
            name "fees-account",
            value "expenses:banking:wise",
            metavar "ACCOUNT_NAME"
          ]
      settingFeeTags <-
        setting
          [ help "Tags for a transaction that was charged a fee",
            conf "fee-tags",
            value ["tax-deductible", "not-vat-deductible"]
          ]
      pure Settings {..}
