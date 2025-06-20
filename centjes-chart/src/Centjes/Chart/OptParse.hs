{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Chart.OptParse
  ( getInstructions,
    Instructions (..),
    Settings (..),
    Dispatch (..),
  )
where

import Centjes.OptParse ()
import Control.Monad.Logger
import Data.Time
import OptEnvConf
import Path
import Path.IO
import Paths_centjes_chart (version)

getInstructions :: IO Instructions
getInstructions = runSettingsParser version "generate reports for swiss taxes"

data Instructions
  = Instructions !Dispatch !Settings

instance HasParser Instructions where
  settingsParser = parseInstructions

{-# ANN parseInstructions ("NOCOVER" :: String) #-}
parseInstructions :: Parser Instructions
parseInstructions =
  subEnv_ "centjes-switzerland" $
    withConfigurableYamlConfig (runIO $ resolveFile' "switzerland.yaml") $
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
  = DispatchAll

instance HasParser Dispatch where
  settingsParser = parseDispatch

{-# ANN parseDispatch ("NOCOVER" :: String) #-}
parseDispatch :: Parser Dispatch
parseDispatch =
  commands
    [ command "all" "produce a all default charts" $ pure DispatchAll,
      defaultCommand "all"
    ]
