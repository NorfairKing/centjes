{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import qualified Data.Text as T
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

data Settings = Settings
  deriving (Show, Eq, Generic)

data Dispatch
  = DispatchBalance !BalanceSettings
  | DispatchFormat !FormatSettings
  deriving (Show, Eq, Generic)

data FormatSettings = FormatSettings
  { formatSettingFileOrDir :: !(Maybe (Either (Path Abs File) (Path Abs Dir)))
  }
  deriving (Show, Eq, Generic)

data BalanceSettings = BalanceSettings
  deriving (Show, Eq, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {}) Environment {} _ = do
  let sets = Settings
  disp <-
    case cmd of
      CommandBalance BalanceArgs -> do
        pure $ DispatchBalance BalanceSettings
      CommandFormat FormatArgs {..} -> do
        formatSettingFileOrDir <- case (formatArgFile, formatArgDir) of
          (Just fp, _) -> Just . Left <$> resolveFile' fp
          (Nothing, Just d) -> Just . Right <$> resolveDir' d
          (Nothing, Nothing) -> pure Nothing
        pure $ DispatchFormat FormatSettings {..}
  pure $ Instructions disp sets

data Configuration = Configuration
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $ pure Configuration

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' "centjes.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "CENTJES_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))

data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = customExecParser prefs_ argParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
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

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandBalance !BalanceArgs
  | CommandFormat !FormatArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "balance" $ CommandBalance <$> parseCommandBalance,
        OptParse.command "format" $ CommandFormat <$> parseCommandFormat
      ]

data BalanceArgs = BalanceArgs
  deriving (Show, Eq, Generic)

parseCommandBalance :: OptParse.ParserInfo BalanceArgs
parseCommandBalance = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Show a balance of accounts"
    parser = pure BalanceArgs

data FormatArgs = FormatArgs
  { formatArgFile :: !(Maybe FilePath),
    formatArgDir :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

parseCommandFormat :: OptParse.ParserInfo FormatArgs
parseCommandFormat = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Format centjes files"
    parser =
      FormatArgs
        <$> optional
          ( strOption
              ( mconcat
                  [ short 'f',
                    long "file",
                    help "Path to file to format"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ short 'd',
                    long "directory",
                    help "Path to directory to format"
                  ]
              )
          )

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

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
