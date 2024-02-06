{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import qualified Centjes.Filter as Filter
import Control.Applicative
import Data.Bool
import Data.Maybe
import qualified Data.Text as T
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
  args@(Arguments _ flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions args env config

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File)
  }
  deriving (Show, Eq, Generic)

data Dispatch
  = DispatchCheck !CheckSettings
  | DispatchRegister !RegisterSettings
  | DispatchBalance !BalanceSettings
  | DispatchFormat !FormatSettings
  deriving (Show, Eq, Generic)

data CheckSettings = CheckSettings
  deriving (Show, Eq, Generic)

data RegisterSettings = RegisterSettings
  { registerSettingFilter :: !Filter,
    registerSettingCurrency :: !(Maybe CurrencySymbol)
  }
  deriving (Show, Eq, Generic)

data BalanceSettings = BalanceSettings
  { balanceSettingFilter :: !Filter,
    balanceSettingCurrency :: !(Maybe CurrencySymbol),
    balanceSettingShowEmpty :: !ShowEmpty
  }
  deriving (Show, Eq, Generic)

data FormatSettings = FormatSettings
  { formatSettingFileOrDir :: !(Maybe (Either (Path Abs File) (Path Abs Dir)))
  }
  deriving (Show, Eq, Generic)

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf = do
  let mLedgerFileOrDir = flagLedgerFile <|> envLedgerFile <|> (mConf >>= configLedgerFile)
  let defaultFilePath = "ledger.cent"
  settingLedgerFile <- case mLedgerFileOrDir of
    Nothing -> resolveFile' defaultFilePath
    Just fod -> do
      f <- resolveFile' fod
      fileExists <- doesFileExist f
      if fileExists
        then pure f
        else do
          d <- resolveDir' fod
          dirExists <- doesDirExist d
          if dirExists
            then resolveFile d defaultFilePath
            else
              die $
                unwords
                  [ "Ledger does not exist at:",
                    fod
                  ]

  disp <-
    case cmd of
      CommandCheck CheckArgs -> do
        pure $ DispatchCheck CheckSettings
      CommandRegister RegisterArgs {..} -> do
        let registerSettingFilter = registerArgFilter
        let registerSettingCurrency = registerArgConversionCurrency
        pure $ DispatchRegister RegisterSettings {..}
      CommandBalance BalanceArgs {..} -> do
        let balanceSettingFilter = balanceArgFilter
        let balanceSettingCurrency = balanceArgConversionCurrency
        let balanceSettingShowEmpty = fromMaybe DoNotShowEmpty balanceArgShowEmpty
        pure $ DispatchBalance BalanceSettings {..}
      CommandFormat FormatArgs {..} -> do
        formatSettingFileOrDir <- case (formatArgFile, formatArgDir) of
          (Just fp, _) -> Just . Left <$> resolveFile' fp
          (Nothing, Just d) -> Just . Right <$> resolveDir' d
          (Nothing, Nothing) -> pure Nothing
        pure $ DispatchFormat FormatSettings {..}
  pure $ Instructions disp Settings {..}

data Configuration = Configuration
  { configLedgerFile :: !(Maybe FilePath)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "ledger" "path to the main ledger file"
          .= configLedgerFile

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
  { envConfigFile :: !(Maybe FilePath),
    envLedgerFile :: !(Maybe FilePath)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser = Env.prefixed "CENTJES_" environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Environment
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "Config file"))
    <*> optional (Env.var Env.str "LEDGER" (Env.help "Ledger file"))

data Arguments
  = Arguments !Command !Flags
  deriving (Show, Eq, Generic)

-- | Get the command-line arguments
getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure ps argParser
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

-- | The @optparse-applicative@ parser for 'Arguments'
argParser :: OptParse.ParserInfo Arguments
argParser =
  OptParse.info
    (OptParse.helper <*> parseArgs)
    OptParse.fullDesc

parseArgs :: OptParse.Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

-- | A sum type for the commands and their specific arguments
data Command
  = CommandCheck !CheckArgs
  | CommandRegister !RegisterArgs
  | CommandBalance !BalanceArgs
  | CommandFormat !FormatArgs
  deriving (Show, Eq, Generic)

parseCommand :: OptParse.Parser Command
parseCommand =
  OptParse.hsubparser $
    mconcat
      [ OptParse.command "check" $ CommandCheck <$> parseCommandCheck,
        OptParse.command "register" $ CommandRegister <$> parseCommandRegister,
        OptParse.command "balance" $ CommandBalance <$> parseCommandBalance,
        OptParse.command "format" $ CommandFormat <$> parseCommandFormat
      ]

data CheckArgs = CheckArgs
  deriving (Show, Eq, Generic)

parseCommandCheck :: OptParse.ParserInfo CheckArgs
parseCommandCheck = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Perform an internal consistency check"
    parser = pure CheckArgs

data RegisterArgs = RegisterArgs
  { registerArgFilter :: !Filter,
    registerArgConversionCurrency :: !(Maybe CurrencySymbol)
  }
  deriving (Show, Eq, Generic)

parseCommandRegister :: OptParse.ParserInfo RegisterArgs
parseCommandRegister = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Show a register of all transactions"
    parser =
      RegisterArgs
        <$> Filter.args
        <*> optional
          ( option
              (eitherReader (CurrencySymbol.fromText . T.pack))
              ( mconcat
                  [ long "convert",
                    metavar "CURRENCY",
                    help "Currency to convert to"
                  ]
              )
          )

data BalanceArgs = BalanceArgs
  { balanceArgFilter :: !Filter,
    balanceArgConversionCurrency :: !(Maybe CurrencySymbol),
    balanceArgShowEmpty :: !(Maybe ShowEmpty)
  }
  deriving (Show, Eq, Generic)

data ShowEmpty
  = ShowEmpty
  | DoNotShowEmpty
  deriving (Show, Eq, Generic)

parseCommandBalance :: OptParse.ParserInfo BalanceArgs
parseCommandBalance = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Show a balance of accounts"
    parser =
      BalanceArgs
        <$> Filter.args
        <*> optional
          ( option
              (eitherReader (CurrencySymbol.fromText . T.pack))
              ( mconcat
                  [ long "convert",
                    metavar "CURRENCY",
                    help "Currency to convert to"
                  ]
              )
          )
        <*> optional
          ( bool ShowEmpty DoNotShowEmpty
              <$> switch
                ( mconcat
                    [ long "show-empty",
                      help "Show empty balances instead of hiding them"
                    ]
                )
          )

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
  { flagConfigFile :: !(Maybe FilePath),
    flagLedgerFile :: !(Maybe FilePath)
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
    <*> optional
      ( strOption
          ( mconcat
              [ long "ledger",
                short 'l',
                help "Path to the main ledger file",
                metavar "FILEPATH"
              ]
          )
      )
