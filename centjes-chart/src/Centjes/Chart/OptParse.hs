{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Chart.OptParse
  ( getInstructions,
    Instructions (..),
    Settings (..),
    Dispatch (..),
    AllSettings (..),
  )
where

import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Filter (Filter)
import Centjes.OptParse ()
import Control.Applicative
import Control.Monad.Logger
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_chart (version)

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "generate charts from a centjes ledger"

data Instructions
  = Instructions !Dispatch !Settings

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions =
  subEnv_ "centjes-chart" $
    withConfigurableYamlConfig (runIO $ resolveFile' "chart.yaml") $
      Instructions
        <$> settingsParser
        <*> settingsParser

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingWatch :: !Bool,
    settingLogLevel :: !LogLevel
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
  settingWatch <-
    setting
      [ help "Run centjes in a loop",
        name "watch",
        short 'w',
        metavar "ANY",
        switch True,
        reader exists,
        value False
      ]
  settingLogLevel <- settingsParser
  pure Settings {..}

data Dispatch
  = DispatchAll !AllSettings

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "all" "produce all default charts" $ DispatchAll <$> parseAllSettings,
      defaultCommand "all"
    ]

data AllSettings = AllSettings
  { allSettingCurrency :: !CurrencySymbol,
    allSettingFilter :: !Filter,
    allSettingOutputFile :: !(Path Abs File),
    allSettingTitle :: !String
  }

{-# ANN parseAllSettings ("NOCOVER" :: String) #-}
parseAllSettings :: Parser AllSettings
parseAllSettings = subConfig_ "all" $ do
  mCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ CurrencySymbol.fromText . T.pack,
          help "Currency to chart in",
          option,
          long "currency",
          conf "currency",
          metavar "CURRENCY"
        ]
  allSettingFilter <- settingsParser
  allSettingOutputFile <-
    filePathSetting
      [ help "Output SVG file",
        short 'o',
        name "output",
        value "assets.svg"
      ]
  mTitle <-
    optional $
      setting
        [ reader str,
          help "Chart title",
          option,
          long "title",
          conf "title",
          metavar "TITLE"
        ]
  pure
    AllSettings
      { allSettingCurrency = fromMaybe (CurrencySymbol "CHF") mCurrency,
        allSettingFilter = allSettingFilter,
        allSettingOutputFile = allSettingOutputFile,
        allSettingTitle = fromMaybe "Assets" mTitle
      }
