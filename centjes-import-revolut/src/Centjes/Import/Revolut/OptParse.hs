{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Import.Revolut.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Centjes.Module
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingInput :: !(Path Abs File),
    settingOutput :: !(Path Abs File),
    settingAssetsAccountName :: !AccountName,
    settingExpensesAccountName :: !AccountName,
    settingFeesAccountName :: !AccountName
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  settingLedgerFile <- resolveFile' $ fromMaybe "ledger.cent" $ flagLedger <|> envLedger <|> mc configLedger
  settingInput <- resolveFile' $ fromMaybe "revolut.cent" $ flagInput <|> envInput
  settingOutput <- resolveFile' $ fromMaybe "revolut.cent" $ flagOutput <|> envOutput
  let settingAssetsAccountName = fromMaybe (AccountName "assets:revolut") $ flagAssetsAccountName <|> envAssetsAccountName <|> mc configAssetsAccountName
  let settingExpensesAccountName = fromMaybe (AccountName "expenses:unknown:revolut") $ flagExpensesAccountName <|> envExpensesAccountName <|> mc configExpensesAccountName
  let settingFeesAccountName = fromMaybe (AccountName "expenses:banking:revolut") $ flagFeesAccountName <|> envFeesAccountName <|> mc configFeesAccountName
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { configLedger :: !(Maybe FilePath),
    configAssetsAccountName :: !(Maybe AccountName),
    configExpensesAccountName :: !(Maybe AccountName),
    configFeesAccountName :: !(Maybe AccountName)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

-- | We use @autodocodec@ for parsing a YAML config.
instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "ledger" "LedgerFile"
          .= configLedger
        <*> optionalField "asset-account" "Asset account name"
          .= configAssetsAccountName
        <*> optionalField "expenses-account" "Expenses account name"
          .= configExpensesAccountName
        <*> optionalField "fees-account" "Fees account name"
          .= configFeesAccountName

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> resolveFile' "revolut.yaml" >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLedger :: !(Maybe FilePath),
    envInput :: !(Maybe FilePath),
    envOutput :: !(Maybe FilePath),
    envAssetsAccountName :: !(Maybe AccountName),
    envExpensesAccountName :: !(Maybe AccountName),
    envFeesAccountName :: !(Maybe AccountName)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "CENTJES_IMPORT_REVOLUT_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
      <*> optional (Env.var Env.str "LEDGER_FILE" (Env.help "Ledger file"))
      <*> optional (Env.var Env.str "INPUT" (Env.help "Input file"))
      <*> optional (Env.var Env.str "OUTPUT" (Env.help "Output file"))
      <*> optional (Env.var (fmap AccountName . Env.str) "ASSETS_ACCOUNT" (Env.help "Assets account name"))
      <*> optional (Env.var (fmap AccountName . Env.str) "EXPENSES_ACCOUNT" (Env.help "Expenses account name"))
      <*> optional (Env.var (fmap AccountName . Env.str) "FEES_ACCOUNT" (Env.help "Fees account name"))

-- | Get the command-line flags
getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Flags'
flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagLedger :: !(Maybe FilePath),
    flagInput :: !(Maybe FilePath),
    flagOutput :: !(Maybe FilePath),
    flagAssetsAccountName :: !(Maybe AccountName),
    flagExpensesAccountName :: !(Maybe AccountName),
    flagFeesAccountName :: !(Maybe AccountName)
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "ledger",
                short 'l',
                help "Ledger file"
              ]
          )
      )
    <*> optional
      ( strArgument
          ( mconcat
              [ help "Input file"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "output",
                short 'o',
                help "Output file"
              ]
          )
      )
    <*> optional
      ( AccountName
          <$> strOption
            ( mconcat
                [ long "asset-account",
                  short 'a',
                  help "Asset account name"
                ]
            )
      )
    <*> optional
      ( AccountName
          <$> strOption
            ( mconcat
                [ long "expenses-account",
                  short 'e',
                  help "Expenses account name"
                ]
            )
      )
    <*> optional
      ( AccountName
          <$> strOption
            ( mconcat
                [ long "fees-account",
                  short 'f',
                  help "Fees account name"
                ]
            )
      )
