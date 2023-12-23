{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Docs.Site.OptParse
  ( module Centjes.Docs.Site.OptParse,
    module Centjes.Docs.Site.OptParse.Types,
  )
where

import Autodocodec.Yaml (readYamlConfigFile)
import Centjes.Docs.Site.OptParse.Types
import Data.Maybe
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Path.IO
import qualified System.Environment as System

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let settingPort = fromMaybe 8080 $ flagPort <|> envPort <|> mc confPort
  let settingGoogleAnalyticsTracking = T.pack <$> (flagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking)
  let settingGoogleSearchConsoleVerification = T.pack <$> (flagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification)
  pure Settings {..}

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error Environment
prefixedEnvironmentParser =
  Env.prefixed
    "CENTJES_DOCS_SITE_"
    environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Environment
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "The config file"))
    <*> optional (Env.var Env.auto "PORT" (Env.help "The port to serve web requests on"))
    <*> optional (Env.var Env.str "GOOGLE_ANALYTICS_TRACKING" (Env.help "The Google analytics tracking code"))
    <*> optional (Env.var Env.str "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (Env.help "The Google search console verification code"))

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> pure Nothing
    Just cf -> resolveFile' cf >>= readYamlConfigFile

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure ps flagsParser
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

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Centjes Docs Site"

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                metavar "FILEPATH",
                help "The config file"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                metavar "PORT",
                help "The port to serve web requests on"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-analytics-tracking",
                metavar "CODE",
                help "The Google analytics tracking code"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-search-console-verification",
                metavar "CODE",
                help "The Google search console verification code"
              ]
          )
      )
