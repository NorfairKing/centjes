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
  { settingBaseDir :: !(Path Abs Dir),
    settingLedgerFile :: !(Path Rel File),
    settingWatch :: !Bool
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings = do
  settingBaseDir <-
    choice
      [ directoryPathSetting
          [ help "base directory",
            name "base-dir"
          ],
        runIO getCurrentDir
      ]
  settingLedgerFile <-
    mapIO parseRelFile $
      setting
        [ help "ledger file",
          reader str,
          short 'l',
          name "ledger",
          value "ledger.cent",
          metavar "FILE_PATH"
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
