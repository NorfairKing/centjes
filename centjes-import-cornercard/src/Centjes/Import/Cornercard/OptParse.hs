{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Cornercard.OptParse where

import Autodocodec
import Autodocodec.Yaml
import qualified Centjes.AccountName as AccountName
import Centjes.Module
import Control.Applicative
import Control.Arrow (left)
import Data.Maybe
import qualified Env
import Options.Applicative as OptParse
import Path
import Path.IO
import qualified System.Environment as System

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
    settingLiabilitiesAccountName :: !AccountName,
    settingExpensesAccountName :: !AccountName,
    settingIncomeAccountName :: !AccountName
  }

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  settingInput <- resolveFile' flagInput
  settingLedgerFile <- resolveFile' $ fromMaybe "ledger.cent" $ flagLedger <|> envLedger <|> mc configLedger
  settingOutput <- resolveFile' $ fromMaybe "cornercard.cent" $ flagOutput <|> envOutput
  let settingLiabilitiesAccountName = fromMaybe "liabilities:cornercard" $ flagLiabilitiesAccountName <|> envLiabilitiesAccountName <|> mc configLiabilitiesAccountName
  let settingExpensesAccountName = fromMaybe "expenses:unknown:cornercard" $ flagExpensesAccountName <|> envExpensesAccountName <|> mc configExpensesAccountName
  let settingIncomeAccountName = fromMaybe "income:unknown:cornercard" $ flagIncomeAccountName <|> envIncomeAccountName <|> mc configIncomeAccountName
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { configLedger :: !(Maybe FilePath),
    configLiabilitiesAccountName :: !(Maybe AccountName),
    configExpensesAccountName :: !(Maybe AccountName),
    configIncomeAccountName :: !(Maybe AccountName)
  }

-- | We use @autodocodec@ for parsing a YAML config.
instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "ledger" "LedgerFile"
          .= configLedger
        <*> optionalField "liabilities-account" "Liabilities account name"
          .= configLiabilitiesAccountName
        <*> optionalField "expenses-account" "Expenses account name"
          .= configExpensesAccountName
        <*> optionalField "income-account" "Income account name"
          .= configIncomeAccountName

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> resolveFile' "cornercard.yaml" >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLedger :: !(Maybe FilePath),
    envOutput :: !(Maybe FilePath),
    envLiabilitiesAccountName :: !(Maybe AccountName),
    envExpensesAccountName :: !(Maybe AccountName),
    envIncomeAccountName :: !(Maybe AccountName)
  }

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

-- | The 'envparse' parser for the 'Environment'
prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "CENTJES_IMPORT_REVOLUT_" environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Environment
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
    <*> optional (Env.var Env.str "LEDGER_FILE" (Env.help "Ledger file"))
    <*> optional (Env.var Env.str "OUTPUT" (Env.help "Output file"))
    <*> optional (Env.var (left Env.UnreadError . AccountName.fromStringOrError) "LIABILITIES_ACCOUNT" (Env.help "Liabilities account name"))
    <*> optional (Env.var (left Env.UnreadError . AccountName.fromStringOrError) "EXPENSES_ACCOUNT" (Env.help "Expenses account name"))
    <*> optional (Env.var (left Env.UnreadError . AccountName.fromStringOrError) "INCOME_ACCOUNT" (Env.help "Income account name"))

-- | Get the command-line flags
getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure ps flagsParser
  where
    ps :: ParserPrefs
    ps =
      prefs $
        mconcat
          [ showHelpOnError,
            showHelpOnEmpty,
            subparserInline,
            helpShowGlobals
          ]

-- | The @optparse-applicative@ parser for 'Flags'
flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    OptParse.fullDesc

-- | The flags that are common across commands.
data Flags = Flags
  { flagInput :: !FilePath,
    flagConfigFile :: !(Maybe FilePath),
    flagLedger :: !(Maybe FilePath),
    flagOutput :: !(Maybe FilePath),
    flagLiabilitiesAccountName :: !(Maybe AccountName),
    flagExpensesAccountName :: !(Maybe AccountName),
    flagIncomeAccountName :: !(Maybe AccountName)
  }

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> strArgument
      ( mconcat
          [ help "Input file",
            metavar "CSV_FILE"
          ]
      )
    <*> optional
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
      ( strOption
          ( mconcat
              [ long "output",
                short 'o',
                help "Output file"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader AccountName.fromStringOrError)
          ( mconcat
              [ long "liabilities-account",
                help "Liabilities account name"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader AccountName.fromStringOrError)
          ( mconcat
              [ long "expenses-account",
                short 'e',
                help "Expenses account name"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader AccountName.fromStringOrError)
          ( mconcat
              [ long "income-account",
                short 'e',
                help "Income account name"
              ]
          )
      )
