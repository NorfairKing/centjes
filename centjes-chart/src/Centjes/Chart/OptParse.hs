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
import Centjes.Validation (renderInvalid)
import Control.Applicative
import Control.Monad.Logger
import Data.Bifunctor (first)
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
  { settingsLedgerFile :: !(Path Abs File),
    settingsWatch :: !Bool,
    settingsLogLevel :: !LogLevel
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingsLedgerFile <-
    filePathSetting
      [ help "ledger file",
        short 'l',
        name "ledger",
        value "ledger.cent"
      ]
  settingsWatch <-
    setting
      [ help "Run centjes in a loop",
        name "watch",
        short 'w',
        metavar "ANY",
        switch True,
        reader exists,
        value False
      ]
  settingsLogLevel <- settingsParser
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
  { allSettingsCurrency :: !CurrencySymbol,
    allSettingsFilter :: !Filter,
    allSettingsOutputFile :: !(Path Abs File),
    allSettingsTitle :: !String
  }

{-# ANN parseAllSettings ("NOCOVER" :: String) #-}
parseAllSettings :: Parser AllSettings
parseAllSettings = subConfig_ "all" $ do
  mCurrency <-
    optional $
      setting
        [ reader $ eitherReader $ first renderInvalid . CurrencySymbol.fromText . T.pack,
          help "Currency to chart in",
          option,
          long "currency",
          conf "currency",
          metavar "CURRENCY"
        ]
  allSettingsFilter <- settingsParser
  allSettingsOutputFile <-
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
      { allSettingsCurrency = fromMaybe (CurrencySymbol "CHF") mCurrency,
        allSettingsFilter = allSettingsFilter,
        allSettingsOutputFile = allSettingsOutputFile,
        allSettingsTitle = fromMaybe "Assets" mTitle
      }
