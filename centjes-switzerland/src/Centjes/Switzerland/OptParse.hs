{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Centjes.AccountName (AccountName)
import Control.Applicative
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text
import Data.Time
import Data.Yaml (FromJSON, ToJSON)
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import Path
import Path.IO
import qualified System.Environment as System
import System.Exit

data Instructions
  = Instructions !Dispatch !Settings
  deriving (Show, Eq, Generic)

getInstructions :: IO Instructions
getInstructions = do
  arguments@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  mConfig <- getConfiguration flags env
  (configPath, config) <- case mConfig of
    Nothing -> die "No config file specified."
    Just (path, config) -> pure (path, config)
  combineToInstructions arguments env configPath config

data Settings = Settings
  { settingBaseDir :: !(Path Abs Dir),
    settingLedgerFile :: !(Path Rel File),
    settingSetup :: !Setup
  }
  deriving (Show, Eq, Generic)

data Dispatch
  = DispatchTaxes !TaxesSettings
  | DispatchVAT !VATSettings
  | DispatchDownloadRates !DownloadRatesSettings
  deriving (Show, Eq, Generic)

data TaxesSettings = TaxesSettings
  { taxesSettingZipFile :: !(Path Abs File),
    taxesSettingReadmeFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

data VATSettings = VATSettings
  { vatSettingZipFile :: !(Path Abs File),
    vatSettingReadmeFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

data DownloadRatesSettings = DownloadRatesSettings
  { downloadRatesSettingBegin :: !Day,
    downloadRatesSettingEnd :: !Day
  }
  deriving (Show, Eq, Generic)

combineToInstructions ::
  Arguments ->
  Environment ->
  Path Abs File ->
  Configuration ->
  IO Instructions
combineToInstructions
  (Arguments cmd Flags {})
  Environment {..}
  configFilePath
  Configuration {..} = do
    let settingBaseDir = parent configFilePath
    settingLedgerFile <- parseRelFile $ fromMaybe "ledger.cent" configLedgerFile
    let settingSetup = configSetup
    dispatch <- case cmd of
      CommandTaxes TaxesArgs {..} -> do
        taxesSettingZipFile <- resolveFile' $ fromMaybe "tax-packet.zip" $ taxesArgZipFile <|> envZipFile <|> configZipFile
        taxesSettingReadmeFile <- resolveFile' $ fromMaybe "README.pdf" $ taxesArgReadmeFile <|> envReadmeFile <|> configReadmeFile
        pure $ DispatchTaxes TaxesSettings {..}
      CommandVAT VATArgs {..} -> do
        vatSettingZipFile <- resolveFile' $ fromMaybe "vat-packet.zip" $ vatArgZipFile <|> envZipFile <|> configZipFile
        vatSettingReadmeFile <- resolveFile' $ fromMaybe "README.pdf" $ vatArgReadmeFile <|> envReadmeFile <|> configReadmeFile
        pure $ DispatchVAT VATSettings {..}
      CommandDownloadRates DownloadRatesArgs {..} -> do
        today <- utctDay <$> getCurrentTime
        let (y, _, _) = toGregorian today
        let downloadRatesSettingBegin = fromMaybe (fromGregorian y 1 1) downloadRatesArgBegin
        let downloadRatesSettingEnd = fromMaybe (addDays (-1) today) downloadRatesArgEnd
        pure $ DispatchDownloadRates DownloadRatesSettings {..}
    pure $ Instructions dispatch Settings {..}

data Configuration = Configuration
  { configZipFile :: !(Maybe FilePath),
    configReadmeFile :: !(Maybe FilePath),
    configLedgerFile :: !(Maybe FilePath),
    configSetup :: !Setup
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "zip" "The zip file to produce"
          .= configZipFile
        <*> optionalField "readme" "The readme file to produce"
          .= configReadmeFile
        <*> optionalField "ledger" "The ledger file"
          .= configLedgerFile
        <*> objectCodec
          .= configSetup

data Setup = Setup
  { setupName :: !Text,
    setupIncomeAccounts :: !(Set AccountName),
    setupAssetsAccounts :: !(Map Text AssetSetup),
    setupExpensesAccounts :: !(Set AccountName)
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
      <*> requiredField "income" "income"
        .= setupIncomeAccounts
      <*> requiredField "assets" "assets"
        .= setupAssetsAccounts
      <*> requiredField "expenses" "expenses"
        .= setupExpensesAccounts

data AssetSetup = AssetSetup
  { assetSetupAccountName :: !AccountName,
    assetSetupEvidence :: !FilePath
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AssetSetup)

instance HasCodec AssetSetup where
  codec =
    object "AssetSetup" $
      AssetSetup
        <$> requiredField "name" "account name"
          .= assetSetupAccountName
        <*> requiredField "evidence" "account statement file"
          .= assetSetupEvidence

getConfiguration :: Flags -> Environment -> IO (Maybe (Path Abs File, Configuration))
getConfiguration Flags {..} Environment {..} = do
  p <- case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile
    Just cf -> resolveFile' cf
  fmap ((,) p) <$> readYamlConfigFile p

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' "switzerland.yaml"

data Environment = Environment
  { envZipFile :: !(Maybe FilePath),
    envReadmeFile :: !(Maybe FilePath),
    envConfigFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "CENTJES_SWITZERLAND_" environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Environment
    <$> optional (Env.var Env.str "ZIP_FILE" (Env.help "Zip file to create"))
    <*> optional (Env.var Env.str "README_FILE" (Env.help "README file to create"))
    <*> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))

data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line flags
getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure ps argumentsParser
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
argumentsParser :: OptParse.ParserInfo Arguments
argumentsParser =
  OptParse.info
    (OptParse.helper <*> parseArguments)
    OptParse.fullDesc

parseArguments :: OptParse.Parser Arguments
parseArguments = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandTaxes !TaxesArgs
  | CommandVAT !VATArgs
  | CommandDownloadRates !DownloadRatesArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "taxes" $ CommandTaxes <$> parseCommandTaxes,
        OptParse.command "vat" $ CommandVAT <$> parseCommandVAT,
        OptParse.command "download-rates" $ CommandDownloadRates <$> parseCommandDownloadRates
      ]

data TaxesArgs = TaxesArgs
  { taxesArgZipFile :: !(Maybe FilePath),
    taxesArgReadmeFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

parseCommandTaxes :: OptParse.ParserInfo TaxesArgs
parseCommandTaxes = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Prepare a tax packet"
    parser =
      TaxesArgs
        <$> optional
          ( strOption
              ( mconcat
                  [ long "zip-file",
                    help "Path to the zip file to create",
                    metavar "FILEPATH"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ long "readme-file",
                    help "Path to the readme file to create",
                    metavar "FILEPATH"
                  ]
              )
          )

data VATArgs = VATArgs
  { vatArgZipFile :: !(Maybe FilePath),
    vatArgReadmeFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

parseCommandVAT :: OptParse.ParserInfo VATArgs
parseCommandVAT = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Prepare a vat packet"
    parser =
      VATArgs
        <$> optional
          ( strOption
              ( mconcat
                  [ long "zip-file",
                    help "Path to the zip file to create",
                    metavar "FILEPATH"
                  ]
              )
          )
        <*> optional
          ( strOption
              ( mconcat
                  [ long "readme-file",
                    help "Path to the readme file to create",
                    metavar "FILEPATH"
                  ]
              )
          )

data DownloadRatesArgs = DownloadRatesArgs
  { downloadRatesArgBegin :: !(Maybe Day),
    downloadRatesArgEnd :: !(Maybe Day)
  }
  deriving (Show, Eq, Generic)

parseCommandDownloadRates :: OptParse.ParserInfo DownloadRatesArgs
parseCommandDownloadRates = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Download exchange rates"
    parser =
      DownloadRatesArgs
        <$> optional
          ( option
              auto
              ( mconcat
                  [ help "The begin date (inclusive), default: Start of the year",
                    metavar "YYYY-MM-DD"
                  ]
              )
          )
        <*> optional
          ( option
              auto
              ( mconcat
                  [ help "The final date (inclusive), default: Yesterday",
                    metavar "YYYY-MM-DD"
                  ]
              )
          )

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
