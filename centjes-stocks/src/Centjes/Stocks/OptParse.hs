{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Stocks.OptParse
  ( getSettings,
    Settings (..),
    Command (..),
    DownloadRatesSettings (..),
    StockConfig (..),
    stockConfigEffectiveTicker,
  )
where

import Autodocodec
import Centjes.CurrencySymbol (CurrencySymbol)
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Time
import OptEnvConf hiding (Command)
import Path
import Path.IO
import Paths_centjes_stocks (version)

getSettings :: IO Settings
getSettings = runSettingsParser version "stock price downloader for centjes"

data Settings = Settings
  { settingLedgerFile :: !(Path Abs File),
    settingCommand :: !Command
  }

instance HasParser Settings where
  settingsParser = parseSettings

{-# ANN parseSettings ("NOCOVER" :: String) #-}
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "centjes-stocks" $
    withConfigurableYamlConfig (runIO $ resolveFile' "stocks.yaml") $ do
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
    [ command "download-rates" "Download stock prices" $
        CommandDownloadRates <$> parseDownloadRatesSettings
    ]

-- | Configuration for a single stock symbol
data StockConfig = StockConfig
  { stockConfigSymbol :: !CurrencySymbol,
    -- | The ticker symbol to use for the Yahoo Finance API (may differ from the currency symbol)
    -- This is Text to allow characters like '.' and '-' that are valid in Yahoo Finance tickers
    stockConfigTicker :: !(Maybe Text),
    stockConfigCurrency :: !CurrencySymbol
  }
  deriving (Show, Eq)

-- | Get the ticker to use for API requests (defaults to symbol text if not specified)
stockConfigEffectiveTicker :: StockConfig -> Text
stockConfigEffectiveTicker StockConfig {..} =
  case stockConfigTicker of
    Just t -> t
    Nothing -> CurrencySymbol.toText stockConfigSymbol

instance HasCodec StockConfig where
  codec =
    object "StockConfig" $
      StockConfig
        <$> requiredField "symbol" "Stock symbol as declared in the ledger (e.g., AAPL)" .= stockConfigSymbol
        <*> optionalField "ticker" "Ticker symbol for Yahoo Finance API (defaults to symbol, e.g., SWDA.L, BRK-B)" .= stockConfigTicker
        <*> requiredField "currency" "Currency the stock is priced in (e.g., USD)" .= stockConfigCurrency

instance HasParser StockConfig where
  settingsParser = parseStockConfig

parseStockConfig :: Parser StockConfig
parseStockConfig = do
  stockConfigSymbol <-
    setting
      [ help "Stock symbol as declared in the ledger (e.g., AAPL)",
        conf "symbol"
      ]
  stockConfigTicker <-
    optional $
      setting
        [ help "Ticker symbol for Yahoo Finance API (defaults to symbol, e.g., SWDA.L, BRK-B)",
          conf "ticker"
        ]
  stockConfigCurrency <-
    setting
      [ help "Currency the stock is priced in (e.g., USD)",
        conf "currency"
      ]
  pure StockConfig {..}

data DownloadRatesSettings = DownloadRatesSettings
  { downloadRatesSettingStocks :: !(NonEmpty StockConfig),
    downloadRatesSettingBegin :: !Day,
    downloadRatesSettingEnd :: !Day,
    downloadRatesSettingOutput :: !(Maybe (Path Abs File))
  }

{-# ANN parseDownloadRatesSettings ("NOCOVER" :: String) #-}
parseDownloadRatesSettings :: Parser DownloadRatesSettings
parseDownloadRatesSettings = subConfig_ "download-rates" $ do
  downloadRatesSettingStocks <-
    setting
      [ help "Stock configurations with symbol and currency",
        conf "stocks"
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
