{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Docs.Site.OptParse.Types where

import Autodocodec
import Data.Text (Text)
import GHC.Generics (Generic)

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagGoogleAnalyticsTracking :: !(Maybe String),
    flagGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envGoogleAnalyticsTracking :: !(Maybe String),
    envGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confGoogleAnalyticsTracking :: !(Maybe String),
    confGoogleSearchConsoleVerification :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "port" "The port on which to serve web requests"
          .= confPort
        <*> optionalFieldOrNull "google-analytics-tracking" "The google analytics tracking code"
          .= confGoogleAnalyticsTracking
        <*> optionalFieldOrNull "google-search-console-verification" "The google search console verification code"
          .= confGoogleSearchConsoleVerification

data Settings = Settings
  { settingPort :: !Int,
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)
