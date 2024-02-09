{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Centjes.AccountName (AccountName)
import Centjes.Switzerland.Report.Taxes.Types (TaxesInput (..))
import Centjes.Switzerland.Report.VAT.Types (VATInput (..))
import Control.Applicative
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.Quarter
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
    taxesSettingReadmeFile :: !(Path Abs File),
    taxesSettingInput :: !TaxesInput
  }
  deriving (Show, Eq, Generic)

data VATSettings = VATSettings
  { vatSettingZipFile :: !(Path Abs File),
    vatSettingReadmeFile :: !(Path Abs File),
    vatSettingInput :: !VATInput
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
  configuration@Configuration {..} = do
    today <- utctDay <$> getCurrentTime
    let settingBaseDir = parent configFilePath
    settingLedgerFile <- parseRelFile $ fromMaybe "ledger.cent" configLedgerFile
    let settingSetup = configSetup
    dispatch <- case cmd of
      CommandTaxes TaxesArgs {..} -> do
        taxesSettingZipFile <- resolveFile' $ fromMaybe "tax-packet.zip" $ taxesArgZipFile <|> envZipFile <|> configZipFile
        taxesSettingReadmeFile <- resolveFile' $ fromMaybe "README.pdf" $ taxesArgReadmeFile <|> envReadmeFile <|> configReadmeFile
        let taxesSettingInput = configureTaxesInput today configuration
        pure $ DispatchTaxes TaxesSettings {..}
      CommandVAT VATArgs {..} -> do
        vatSettingZipFile <- resolveFile' $ fromMaybe "vat-packet.zip" $ vatArgZipFile <|> envZipFile <|> configZipFile
        vatSettingReadmeFile <- resolveFile' $ fromMaybe "README.pdf" $ vatArgReadmeFile <|> envReadmeFile <|> configReadmeFile
        let vatSettingInput = configureVATInput today configuration
        pure $ DispatchVAT VATSettings {..}
      CommandDownloadRates DownloadRatesArgs {..} -> do
        let (y, _, _) = toGregorian today
        let downloadRatesSettingBegin = fromMaybe (fromGregorian y 1 1) downloadRatesArgBegin
        let downloadRatesSettingEnd = fromMaybe (addDays (-1) today) downloadRatesArgEnd
        pure $ DispatchDownloadRates DownloadRatesSettings {..}
    pure $ Instructions dispatch Settings {..}

configureVATInput :: Day -> Configuration -> VATInput
configureVATInput day Configuration {..} =
  let vatInputPersonName = T.unwords [configFirstName, configLastName]
      vatInputOrganisationName = configOrganisationName
      vatInputVATId = configVATId
      currentQuarter = dayPeriod day
      vatInputQuarter = fromMaybe currentQuarter configQuarter
      vatInputDomesticIncomeAccountName = fromMaybe "income:domestic" configDomesticIncomeAccountName
      vatInputExportsIncomeAccountName = fromMaybe "income:exports" configExportsIncomeAccountName
      vatInputForeignIncomeAccountName = fromMaybe "income:foreign" configForeignIncomeAccountName
      vatInputVATIncomeAccountName = fromMaybe "income:vat" configVATIncomeAccountName
      vatInputVATExpensesAccountName = fromMaybe "expenses:vat" configVATExpensesAccount
   in VATInput {..}

configureTaxesInput :: Day -> Configuration -> TaxesInput
configureTaxesInput day Configuration {..} =
  let taxesInputLastName = configLastName
      taxesInputFirstName = configFirstName
      (currentYear, _, _) = toGregorian day
      taxesInputYear = fromMaybe currentYear configYear
      taxesInputInsuredPersonNumber = configInsuredPersonNumber
   in TaxesInput {..}

data Configuration = Configuration
  { configZipFile :: !(Maybe FilePath),
    configReadmeFile :: !(Maybe FilePath),
    configLedgerFile :: !(Maybe FilePath),
    configLastName :: !Text,
    configFirstName :: !Text,
    configOrganisationName :: !Text,
    configVATId :: !Text,
    configInsuredPersonNumber :: !Text,
    configYear :: !(Maybe Year),
    configQuarter :: !(Maybe Quarter),
    configDomesticIncomeAccountName :: !(Maybe AccountName),
    configExportsIncomeAccountName :: !(Maybe AccountName),
    configForeignIncomeAccountName :: !(Maybe AccountName),
    configVATIncomeAccountName :: !(Maybe AccountName),
    configVATExpensesAccount :: !(Maybe AccountName),
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
        <*> requiredField "last-name" "Your last name"
          .= configLastName
        <*> requiredField "first-name" "Your first name"
          .= configFirstName
        <*> requiredField "organisation-name" "The organisation's legal name"
          .= configOrganisationName
        <*> requiredField "vat-id" "The VAT identifier. e.g. 111.222.333"
          .= configVATId
        <*> requiredField "ahv-id" "The AHV identifier. e.g. 746.1111.2222.33"
          .= configInsuredPersonNumber
        <*> optionalField "year" "The year to produce a taxes report for"
          .= configYear
        <*> optionalFieldWith "quarter" (codecViaAeson "Quarter") "The quarter to produce a vat report for"
          .= configQuarter
        <*> optionalField "domestic-income-account" "Account name of your domestic income"
          .= configDomesticIncomeAccountName
        <*> optionalField "exports-income-account" "Account name of your exports' income"
          .= configDomesticIncomeAccountName
        <*> optionalField "foreign-income-account" "Account name of your foreign income"
          .= configDomesticIncomeAccountName
        <*> optionalField "vat-income-account" "Account name of your the VAT you've charged"
          .= configVATIncomeAccountName
        <*> optionalField "vat-expenses-account" "Account name of your the VAT you've paid"
          .= configVATExpensesAccount
        <*> objectCodec
          .= configSetup

data Setup = Setup
  { setupIncomeAccounts :: !(Map AccountName IncomeSetup),
    setupAssetsAccounts :: !(Map Text AssetSetup),
    setupExpensesAccounts :: !(Set AccountName),
    setupVATIncomeAccount :: !AccountName
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Setup)

instance HasCodec Setup where
  codec = object "Setup" objectCodec

instance HasObjectCodec Setup where
  objectCodec =
    Setup
      <$> requiredField "income" "income"
        .= setupIncomeAccounts
      <*> requiredField "assets" "assets"
        .= setupAssetsAccounts
      <*> requiredField "expenses" "expenses"
        .= setupExpensesAccounts
      <*> requiredField "vat-income" "VAT income account"
        .= setupVATIncomeAccount

data IncomeSetup = IncomeSetup
  { incomeSetupForeign :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec IncomeSetup)

instance HasCodec IncomeSetup where
  codec =
    object "IncomeSetup" $
      IncomeSetup
        <$> requiredField "foreign" "whether the income is foreign"
          .= incomeSetupForeign

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
        <$> requiredField "name" "Account name of the account"
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
