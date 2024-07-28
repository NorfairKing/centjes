{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Docs.Site.OptParse
  ( getSettings,
    Settings (..),
  )
where

import Data.Text (Text)
import OptEnvConf
import Paths_centjes_docs_site

getSettings :: IO Settings
getSettings = runSettingsParser version "centjes docs site"

data Settings = Settings
  { settingPort :: !Int,
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }

instance HasParser Settings where
  settingsParser =
    subEnv_ "centjes-docs-site" $
      withLocalYamlConfig $ do
        settingPort <-
          setting
            [ help "port to serve web requests on on",
              reader auto,
              name "port",
              value 8080,
              metavar "PORT"
            ]
        settingGoogleAnalyticsTracking <-
          optional $
            setting
              [ help "The Google analytics tracking code",
                reader str,
                name "google-analytics-tracking",
                metavar "CODE"
              ]
        settingGoogleSearchConsoleVerification <-
          optional $
            setting
              [ help "The Google search console verification code",
                reader str,
                name "google-search-console-verification",
                metavar "CODE"
              ]
        pure Settings {..}
