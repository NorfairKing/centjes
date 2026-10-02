{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Wise.OptParse
  ( getSettings,
    Settings (..),
    Command (..),
    FetchSettings (..),
    ImportSettings (..),
    importDeclarationSettings,
  )
where

import qualified Centjes.AccountName as AccountName
import Centjes.Import.Wise.API (WiseApiToken (..))
import Centjes.Import.Wise.Declarations (DeclarationSettings (..))
import Centjes.Module
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Time
import OptEnvConf hiding (Command)
import Path
import Path.IO
import Paths_centjes_import_wise (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "importer for wise"

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingCommand :: !Command
  }

instance HasParser Settings where
  settingsParser = parseSettings

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
      settingCommand <- settingsCommandParser
      pure Settings {..}

data Command
  = CommandFetch !FetchSettings
  | CommandImport !ImportSettings

settingsCommandParser :: Parser Command
settingsCommandParser =
  commands
    [ command "fetch" "Download a statement for every balance" $
        CommandFetch <$> parseFetchSettings,
      command "import" "Import downloaded or exported statements" $
        CommandImport <$> parseImportSettings
    ]

data FetchSettings = FetchSettings
  { fetchSettingToken :: !WiseApiToken,
    -- | The key that answers Wise's authentication challenge.
    --
    -- Optional because the challenge only happens for some accounts, and an
    -- account that is never challenged needs no key.
    fetchSettingPrivateKeyFile :: !(Maybe (Path Abs File)),
    fetchSettingBaseUrl :: !String,
    fetchSettingOutputDirectory :: !(Path Abs Dir),
    -- | The profile to fetch, or every profile the token can see.
    fetchSettingProfile :: !(Maybe Int64),
    -- | The first day to fetch, which defaults to the start of this year.
    fetchSettingBegin :: !(Maybe Day),
    -- | The last day to fetch, which defaults to today.
    fetchSettingEnd :: !(Maybe Day)
  }

{-# ANN parseFetchSettings ("NOCOVER" :: String) #-}
parseFetchSettings :: Parser FetchSettings
parseFetchSettings = subConfig_ "fetch" $ do
  fetchSettingToken <-
    WiseApiToken
      <$> secretTextFileOrBareSetting
        [ help "Wise API token that can read your balances and statements",
          name "token"
        ]
  fetchSettingPrivateKeyFile <-
    optional $
      filePathSetting
        [ help "PEM file holding the private key registered with Wise, which signs its authentication challenge",
          name "private-key-file",
          metavar "FILE_PATH"
        ]
  fetchSettingBaseUrl <-
    setting
      [ help "Base URL of the Wise API, which the sandbox and the live API differ in",
        reader str,
        name "base-url",
        value "https://api.wise.com",
        metavar "URL"
      ]
  fetchSettingOutputDirectory <-
    directoryPathSetting
      [ help "Directory to download the statements into",
        short 'd',
        name "statement-directory",
        value "wise-statements",
        metavar "DIRECTORY_PATH"
      ]
  fetchSettingProfile <-
    optional $
      setting
        [ help "Only fetch the balances of this profile, instead of every profile",
          reader auto,
          name "profile",
          metavar "PROFILE_ID"
        ]
  fetchSettingBegin <-
    optional $
      setting
        [ help "First day to fetch, which defaults to the first day of this year",
          reader $ maybeReader (parseTimeM True defaultTimeLocale "%F"),
          name "begin",
          metavar "YYYY-MM-DD"
        ]
  fetchSettingEnd <-
    optional $
      setting
        [ help "Last day to fetch, which defaults to today",
          reader $ maybeReader (parseTimeM True defaultTimeLocale "%F"),
          name "end",
          metavar "YYYY-MM-DD"
        ]
  pure FetchSettings {..}

data ImportSettings = ImportSettings
  { -- | The statements to read.
    --
    -- More than one, because a conversion is written into the statement of both
    -- balances it moved between, and only reading both makes it one
    -- transaction rather than two halves.
    importSettingInputs :: !(NonEmpty (Path Abs File)),
    importSettingOutput :: !(Path Abs File),
    importSettingAssetsAccountName :: !AccountName,
    importSettingExpensesAccountName :: !AccountName,
    importSettingIncomeAccountName :: !AccountName,
    importSettingFeesAccountName :: !AccountName,
    importSettingFeeTags :: ![Tag]
  }

-- | The part of the import settings the emitter needs.
importDeclarationSettings :: ImportSettings -> DeclarationSettings
importDeclarationSettings ImportSettings {..} =
  DeclarationSettings
    { declarationSettingAssetsAccountName = importSettingAssetsAccountName,
      declarationSettingExpensesAccountName = importSettingExpensesAccountName,
      declarationSettingIncomeAccountName = importSettingIncomeAccountName,
      declarationSettingFeesAccountName = importSettingFeesAccountName,
      declarationSettingFeeTags = importSettingFeeTags
    }

{-# ANN parseImportSettings ("NOCOVER" :: String) #-}
parseImportSettings :: Parser ImportSettings
parseImportSettings = subConfig_ "import" $ do
  importSettingInputs <-
    someNonEmpty $
      filePathSetting
        [ help "statement to import, one per balance",
          argument,
          metavar "CSV_FILE"
        ]
  importSettingOutput <-
    filePathSetting
      [ help "output ledger file, which this importer rewrites",
        short 'o',
        name "output",
        value "wise-import.cent",
        metavar "FILE_PATH"
      ]
  importSettingAssetsAccountName <-
    setting
      [ help "Assets account name, which holds every currency",
        reader $ eitherReader AccountName.fromStringOrError,
        name "assets-account",
        value "assets:wise",
        metavar "ACCOUNT_NAME"
      ]
  importSettingExpensesAccountName <-
    setting
      [ help "Expenses account name",
        reader $ eitherReader AccountName.fromStringOrError,
        name "expenses-account",
        value "expenses:unknown:wise",
        metavar "ACCOUNT_NAME"
      ]
  importSettingIncomeAccountName <-
    setting
      [ help "Income account name",
        reader $ eitherReader AccountName.fromStringOrError,
        name "income-account",
        value "income:unknown:wise",
        metavar "ACCOUNT_NAME"
      ]
  importSettingFeesAccountName <-
    setting
      [ help "Fees account name",
        reader $ eitherReader AccountName.fromStringOrError,
        name "fees-account",
        value "expenses:banking:wise",
        metavar "ACCOUNT_NAME"
      ]
  importSettingFeeTags <-
    setting
      [ help "Tags for a transaction that was charged a fee",
        conf "fee-tags",
        value ["tax-deductible", "not-vat-deductible"]
      ]
  pure ImportSettings {..}
