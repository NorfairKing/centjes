{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Cryptocurrencies.OptParse
  ( getSettings,
    Settings (..),
    Command (..),
    DownloadRatesSettings (..),
  )
where

import Centjes.CurrencySymbol (CurrencySymbol)
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time
import OptEnvConf hiding (Command)
import Path
import Path.IO
import Paths_centjes_cryptocurrencies (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "cryptocurrency exchange rate downloader for centjes"

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingCommand :: !Command
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-cryptocurrencies" $
    withConfigurableYamlConfig (runIO $ resolveFile' "cryptocurrencies.yaml") $ do
      settingLedgerFile <-
        filePathSetting
          [ help "ledger file",
            short 'l',
            name "ledger",
            value "ledger.cent",
            metavar "FILE_PATH"
          ]
      settingCommand <- settingsCommandParser
      pure Settings {..}

newtype Command
  = CommandDownloadRates DownloadRatesSettings

settingsCommandParser :: Parser Command
settingsCommandParser =
  commands
    [ command "download-rates" "Download cryptocurrency exchange rates" $
        CommandDownloadRates <$> parseDownloadRatesSettings
    ]

data DownloadRatesSettings = DownloadRatesSettings
  { downloadRatesSettingSymbols :: !(Maybe (NonEmpty CurrencySymbol)),
    downloadRatesSettingTarget :: !CurrencySymbol,
    downloadRatesSettingBegin :: !Day,
    downloadRatesSettingEnd :: !Day,
    downloadRatesSettingOutput :: !(Maybe (Path Abs File))
  }

{-# ANN parseDownloadRatesSettings ("NOCOVER" :: String) #-}
parseDownloadRatesSettings :: Parser DownloadRatesSettings
parseDownloadRatesSettings = subConfig_ "download-rates" $ do
  downloadRatesSettingSymbols <-
    optional $
      setting
        [ help "Comma-separated cryptocurrency symbols to download (default: all currencies from ledger)",
          reader $ eitherReader parseSymbolsList,
          option,
          long "symbols",
          metavar "SYMBOLS"
        ]
  downloadRatesSettingTarget <-
    setting
      [ help "Target currency symbol",
        reader $ eitherReader (CurrencySymbol.fromText . T.pack),
        option,
        long "target",
        name "target",
        value (CurrencySymbol.CurrencySymbol "USD"),
        metavar "CURRENCY"
      ]
  downloadRatesSettingBegin <-
    choice
      [ setting
          [ help "The begin date (inclusive), default: Start of the year",
            reader auto,
            name "begin",
            metavar "YYYY-MM-DD"
          ],
        runIO $ do
          today <- utctDay <$> getCurrentTime
          let (y, _, _) = toGregorian today
          pure $ fromGregorian y 1 1
      ]
  downloadRatesSettingEnd <-
    choice
      [ setting
          [ help "The final date (inclusive), default: Yesterday",
            reader auto,
            name "end",
            metavar "YYYY-MM-DD"
          ],
        runIO $ addDays (-1) . utctDay <$> getCurrentTime
      ]
  downloadRatesSettingOutput <-
    optional $
      filePathSetting
        [ help "Output file path (default: stdout)",
          short 'o',
          name "output",
          metavar "FILE"
        ]
  pure DownloadRatesSettings {..}

parseSymbolsList :: String -> Either String (NonEmpty CurrencySymbol)
parseSymbolsList s = do
  let parts = map T.strip $ T.splitOn "," $ T.pack s
  case NE.nonEmpty parts of
    Nothing -> Left "Symbols list cannot be empty"
    Just neParts
      | any T.null neParts -> Left "Symbols list cannot contain empty symbols"
      | otherwise -> traverse CurrencySymbol.fromText neParts
