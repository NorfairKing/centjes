{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Centjes.AccountName (AccountName)
import Control.Applicative
import Data.Maybe
import Data.Set (Set)
import Data.Text
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import Path
import Path.IO
import System.Exit

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  mConfig <- getConfiguration flags env
  (configPath, config) <- case mConfig of
    Nothing -> die "No config file specified."
    Just (path, config) -> pure (path, config)
  combineToSettings configPath config

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingSetup :: !Setup
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Path Abs File -> Configuration -> IO Settings
combineToSettings configFilePath Configuration {..} = do
  settingLedgerFile <- resolveFile (parent configFilePath) $ fromMaybe "ledger.cent" configLedgerFile
  let settingSetup = configSetup
  pure Settings {..}

data Configuration = Configuration
  { configLedgerFile :: !(Maybe FilePath),
    configSetup :: !Setup
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "ledger" "The ledger file"
          .= configLedgerFile
        <*> objectCodec
          .= configSetup

data Setup = Setup
  { setupName :: !Text,
    setupIncomeAccounts :: Set AccountName
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Setup)

instance HasCodec Setup where
  codec = object "Setup" objectCodec

instance HasObjectCodec Setup where
  objectCodec =
    Setup
      <$> requiredField "name" "name"
        .= setupName
      <*> requiredField "income-accounts" "income accounts"
        .= setupIncomeAccounts

getConfiguration :: Flags -> Environment -> IO (Maybe (Path Abs File, Configuration))
getConfiguration Flags {..} Environment {..} = do
  p <- case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile
    Just cf -> resolveFile' cf
  fmap ((,) p) <$> readYamlConfigFile p

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' "centjes-switzerland.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "CENTJES_SWITZERLAND_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))

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
    OptParse.fullDesc

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath)
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