{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.OptParse where

import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import Control.Applicative
import qualified Data.Text as T
import OptEnvConf
import Path
import Path.IO
import Paths_centjes (version)

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "really safe double-entry accounting"

data Instructions
  = Instructions !Dispatch !Settings

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions =
  subEnv_ "centjes" $
    withConfigurableYamlConfig (runIO $ resolveFile' "centjes.yaml") $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File)
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingLedgerFile <-
    filePathSetting
      [ help "ledger file",
        short 'l',
        name "ledger",
        value "ledger.cent"
      ]
  pure Settings {..}

data Dispatch
  = DispatchCheck !CheckSettings
  | DispatchRegister !RegisterSettings
  | DispatchBalance !BalanceSettings
  | DispatchFormat !FormatSettings

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "check" "perform an internal consistency check" $ DispatchCheck <$> settingsParser,
      command "register" "register report" $ DispatchRegister <$> settingsParser,
      command "balance" "balance report" $ DispatchBalance <$> settingsParser,
      command "format" "format files" $ DispatchFormat <$> settingsParser
    ]

data CheckSettings = CheckSettings

instance HasParser CheckSettings where
  settingsParser = parseCheckSettings

{-# ANN parseCheckSettings ("NOCOVER" :: String) #-}
parseCheckSettings :: Parser CheckSettings
parseCheckSettings = pure CheckSettings

data RegisterSettings = RegisterSettings
  { registerSettingFilter :: !Filter,
    registerSettingCurrency :: !(Maybe CurrencySymbol)
  }

instance HasParser RegisterSettings where
  settingsParser = parseRegisterSettings

{-# ANN parseRegisterSettings ("NOCOVER" :: String) #-}
parseRegisterSettings :: Parser RegisterSettings
parseRegisterSettings = do
  registerSettingFilter <- settingsParser
  registerSettingCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to convert to",
          option,
          long "convert",
          metavar "CURRENCY"
        ]
  pure RegisterSettings {..}

data BalanceSettings = BalanceSettings
  { balanceSettingFilter :: !Filter,
    balanceSettingCurrency :: !(Maybe CurrencySymbol),
    balanceSettingShowEmpty :: !ShowEmpty,
    balanceSettingShowVirtual :: !Bool
  }

instance HasParser BalanceSettings where
  settingsParser = parseBalanceSettings

{-# ANN parseBalanceSettings ("NOCOVER" :: String) #-}
parseBalanceSettings :: Parser BalanceSettings
parseBalanceSettings = do
  balanceSettingFilter <- settingsParser
  balanceSettingCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to convert to",
          option,
          long "convert",
          metavar "CURRENCY"
        ]
  balanceSettingShowEmpty <-
    setting
      [ help "Show empty balances instead of hiding them",
        switch ShowEmpty,
        long "show-empty",
        value DoNotShowEmpty
      ]
  balanceSettingShowVirtual <-
    setting
      [ help "Show virtual postings too",
        switch True,
        long "virtual",
        value False
      ]
  pure BalanceSettings {..}

data ShowEmpty
  = ShowEmpty
  | DoNotShowEmpty
  deriving (Show)

data FormatSettings = FormatSettings
  { formatSettingFileOrDir :: !(Maybe (Either (Path Abs File) (Path Abs Dir)))
  }

instance HasParser FormatSettings where
  settingsParser = parseFormatSettings

{-# ANN parseFormatSettings ("NOCOVER" :: String) #-}
parseFormatSettings :: Parser FormatSettings
parseFormatSettings = do
  formatSettingFileOrDir <-
    optional $
      choice
        [ Left
            <$> filePathSetting
              [ help "File to format",
                option,
                short 'f',
                long "file"
              ],
          Right
            <$> directoryPathSetting
              [ help "Directory to format",
                option,
                short 'd',
                long "directory"
              ]
        ]
  pure FormatSettings {..}

--       BalanceArgs
--         <$> Filter.args
