{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Centjes.Stocks.Command.DownloadRates (runCentjesStocksDownloadRates) where

import Centjes.Compile
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format (formatModule)
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Stocks.OptParse
import Centjes.Validation
import Conduit
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (forM_, guard, unless, when)
import Control.Monad.Logger
import Data.Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Data.Vector as V
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Path
import System.Exit (exitFailure)
import Text.Printf (printf)

runCentjesStocksDownloadRates :: Settings -> DownloadRatesSettings -> IO ()
runCentjesStocksDownloadRates Settings {..} DownloadRatesSettings {..} = runStderrLoggingT $ do
  -- Load ledger to get declared currencies
  (declarations, diag) <- loadModules settingLedgerFile
  currencies <- liftIO $ checkValidation diag $ compileDeclarationsCurrencies declarations

  let stocks = NE.toList downloadRatesSettingStocks

  man <- liftIO newTlsManager

  -- Single unified conduit pipeline
  generatedDeclarations <-
    runConduit $
      yieldMany stocks
        -- Validate symbol and target currency are declared in ledger
        .| C.mapM (validateCurrenciesDeclared currencies)
        -- Throttle requests (500ms between each)
        .| C.mapM (\sc -> liftIO (threadDelay 500_000) >> pure sc)
        -- Fetch and validate immediately (fail-fast on error)
        .| C.mapM (fetchAndValidate man downloadRatesSettingBegin downloadRatesSettingEnd)
        -- Validate data exists in date range
        .| C.mapM (validateHasData downloadRatesSettingBegin downloadRatesSettingEnd)
        -- Expand to (symbol, targetCurrency, day, price) for each data point
        .| C.concatMap (\(sym, targetCur, history) -> [(sym, targetCur, day, price) | (day, price) <- stockHistoryTimeSeries history])
        -- Filter to requested date range
        .| C.filter (\(_, _, day, _) -> day >= downloadRatesSettingBegin && day <= downloadRatesSettingEnd)
        -- Convert to rate expressions
        .| C.concatMap (\(sym, targetCur, day, price) -> (sym,targetCur,day,) <$> priceToRateExpression price)
        -- Generate price declarations
        .| C.map
          ( \(sym, targetCurrency, day, rateExpression) ->
              (day, noLoc $ DeclarationPrice $ noLoc $ mkPriceDeclaration day sym rateExpression targetCurrency)
          )
        .| C.sinkList

  when (null generatedDeclarations) $ do
    logErrorN "No price declarations were generated"
    liftIO exitFailure

  -- Sort by date to ensure chronological order (required by centjes)
  let output =
        TE.encodeUtf8 $
          formatModule $
            Module
              { moduleImports = [],
                moduleDeclarations = map snd $ sortOn fst generatedDeclarations
              }

  liftIO $ case downloadRatesSettingOutput of
    Nothing -> SB.putStr output
    Just outputFile -> SB.writeFile (fromAbsFile outputFile) output

-- | Yahoo Finance API response structure
newtype YahooChartResponse = YahooChartResponse YahooChartResult

instance FromJSON YahooChartResponse where
  parseJSON = withObject "YahooChartResponse" $ \o -> do
    chart <- o .: "chart"
    result <- chart .: "result"
    case result of
      [] -> fail "No results in Yahoo Finance response"
      (r : _) -> pure $ YahooChartResponse r

-- | Yahoo Finance chart result with currency info and price data
-- Fields: currency, timestamps, close prices
data YahooChartResult = YahooChartResult !T.Text !(V.Vector Integer) !(V.Vector (Maybe Scientific))

instance FromJSON YahooChartResult where
  parseJSON = withObject "YahooChartResult" $ \o -> do
    meta <- o .: "meta"
    currency <- meta .: "currency"
    timestamps <- o .: "timestamp"
    indicators <- o .: "indicators"
    quote <- indicators .: "quote"
    case quote of
      [] -> fail "No quote data in Yahoo Finance response"
      (q : _) -> do
        closePrices <- q .: "close"
        pure $ YahooChartResult currency timestamps closePrices

-- | Stock history data with the actual currency from the API
data StockHistory = StockHistory
  { stockHistoryApiCurrency :: !T.Text, -- The currency code from Yahoo Finance (e.g., "GBp", "USD")
    stockHistoryTimeSeries :: ![(Day, Scientific)] -- Prices converted to base currency
  }

-- | Get the divisor and base currency for a currency code returned by Yahoo Finance
-- Some exchanges quote prices in minor currency units (e.g., pence instead of pounds)
-- Returns (divisor, baseCurrency)
currencyInfo :: T.Text -> (Scientific, T.Text)
currencyInfo currency
  -- Yahoo Finance uses lowercase 'p' suffix for pence (e.g., "GBp" for British pence)
  | T.length currency == 3 && T.last currency == 'p' =
      let base = T.toUpper (T.init currency) <> "P"
       in (100, base)
  | otherwise = case T.toLower currency of
      "gbp" -> (1, "GBP") -- British Pounds
      "gbx" -> (100, "GBP") -- British Pence (100 pence = 1 GBP)
      "ila" -> (100, "ILS") -- Israeli Agorot (100 agorot = 1 ILS)
      "zac" -> (100, "ZAR") -- South African Cents (100 cents = 1 ZAR)
      _ -> (1, T.toUpper currency)

-- | Fetch historical stock prices from Yahoo Finance
-- Takes a ticker (Text) which may contain special characters like '.' or '-'
-- Returns the stock history including the API currency for validation
fetchStockHistory :: Manager -> T.Text -> Day -> Day -> LoggingT IO (Maybe StockHistory)
fetchStockHistory man ticker begin end = do
  let period1 = dayToUnixTimestamp begin
  let period2 = dayToUnixTimestamp (addDays 1 end) -- Add 1 day to make end inclusive
  let url =
        "https://query1.finance.yahoo.com/v8/finance/chart/"
          <> T.unpack ticker
          <> "?period1="
          <> show period1
          <> "&period2="
          <> show period2
          <> "&interval=1d"

  logDebugN $ "Fetching " <> T.pack url
  result <- liftIO $ try $ do
    request <- HTTP.parseRequest url
    let requestWithHeaders =
          request
            { requestHeaders =
                [ ("User-Agent", "Mozilla/5.0 (compatible; centjes-stocks/1.0)")
                ]
            }
    HTTP.httpLbs requestWithHeaders man

  case result of
    Left (e :: HTTP.HttpException) -> do
      logWarnN $ "HTTP request failed for " <> ticker <> ": " <> T.pack (show e)
      pure Nothing
    Right response ->
      if HTTP.statusCode (responseStatus response) /= 200
        then do
          logWarnN $ "HTTP request for " <> ticker <> " returned status " <> T.pack (show (HTTP.statusCode (responseStatus response)))
          pure Nothing
        else pure $ parseYahooResponse (responseBody response)

-- | Validate that symbol and target currency are declared in the ledger
validateCurrenciesDeclared ::
  M.Map CurrencySymbol a ->
  StockSettings ->
  LoggingT IO StockSettings
validateCurrenciesDeclared currencies sc = do
  let sym = stockSettingsSymbol sc
  unless (M.member sym currencies) $ do
    logErrorN $ "Symbol " <> CurrencySymbol.toText sym <> " is not declared as a currency in the ledger"
    logErrorN $ "Please add a currency declaration (e.g., 'currency " <> CurrencySymbol.toText sym <> " 0.01')"
    liftIO exitFailure
  let targetCurrency = stockSettingsCurrency sc
  unless (M.member targetCurrency currencies) $ do
    logErrorN $ "Target currency " <> CurrencySymbol.toText targetCurrency <> " is not declared in the ledger"
    liftIO exitFailure
  pure sc

-- | Fetch and validate stock history for a stock config (fail-fast on error)
fetchAndValidate ::
  Manager ->
  Day ->
  Day ->
  StockSettings ->
  LoggingT IO (CurrencySymbol, CurrencySymbol, StockHistory)
fetchAndValidate man begin end sc = do
  let ticker = stockSettingsEffectiveTicker sc
  let sym = stockSettingsSymbol sc
  let configuredCurrency = stockSettingsCurrency sc
  mHistory <- fetchStockHistory man ticker begin end
  case mHistory of
    Nothing -> do
      logErrorN $ "Failed to download data for ticker: " <> ticker
      liftIO exitFailure
    Just history -> do
      let apiCurrency = stockHistoryApiCurrency history
      let (_, baseCurrency) = currencyInfo apiCurrency
      when (T.toUpper baseCurrency /= T.toUpper (CurrencySymbol.toText configuredCurrency)) $ do
        logErrorN $ "Currency mismatch for " <> ticker <> ": API returns " <> apiCurrency <> " (base: " <> baseCurrency <> "), but configured as " <> CurrencySymbol.toText configuredCurrency
        liftIO exitFailure
      pure (sym, configuredCurrency, history)

-- | Validate that history has data in the requested date range
validateHasData ::
  Day ->
  Day ->
  (CurrencySymbol, CurrencySymbol, StockHistory) ->
  LoggingT IO (CurrencySymbol, CurrencySymbol, StockHistory)
validateHasData begin end (sym, targetCurrency, history) = do
  let hasDataInRange = any (\(day, _) -> day >= begin && day <= end) (stockHistoryTimeSeries history)
  unless hasDataInRange $ do
    logErrorN $ "No price data found for " <> CurrencySymbol.toText sym <> " in the requested date range"
    liftIO exitFailure
  pure (sym, targetCurrency, history)

-- | Convert a Day to Unix timestamp (seconds since epoch)
dayToUnixTimestamp :: Day -> Integer
dayToUnixTimestamp day =
  let utcTime = UTCTime day 0
   in floor $ utcTimeToPOSIXSeconds utcTime

-- | Convert Unix timestamp to Day
unixTimestampToDay :: Integer -> Day
unixTimestampToDay ts =
  let utcTime = posixSecondsToUTCTime (fromInteger ts)
   in utctDay utcTime

-- | Parse Yahoo Finance JSON response into stock history data
-- Automatically converts prices from minor currency units (e.g., pence) to major units (e.g., pounds)
parseYahooResponse :: LB.ByteString -> Maybe StockHistory
parseYahooResponse body = do
  YahooChartResponse (YahooChartResult apiCurrency timestamps closePrices) <- decode body
  let (divisor, _baseCurrency) = currencyInfo apiCurrency
      timestampList = V.toList timestamps
      priceList = V.toList closePrices
      pricePairs = zip timestampList priceList
      timeSeries = [(unixTimestampToDay ts, price / divisor) | (ts, Just price) <- pricePairs]
  pure $ StockHistory apiCurrency timeSeries

-- | Convert a stock price to a rate expression
-- The price is directly the value in the target currency (e.g., USD)
priceToRateExpression :: Scientific -> Maybe (RationalExpression ())
priceToRateExpression price = do
  guard (price > 0)
  let priceDouble = toRealFloat price :: Double
  -- Use 2 decimals for prices >= 1
  -- For prices < 1, use more precision
  let decimals
        | priceDouble >= 1 = 2
        | otherwise = countLeadingZeroDecimals priceDouble + 4
  rateLiteral <- DecimalLiteral.fromString $ printf "%.*f" decimals priceDouble
  pure
    RationalExpression
      { rationalExpressionNumerator = noLoc rateLiteral,
        rationalExpressionDenominator = Nothing,
        rationalExpressionPercent = False
      }

-- | Count the number of leading zero decimals (e.g., 0.0065 has 2)
countLeadingZeroDecimals :: Double -> Int
countLeadingZeroDecimals x
  | x >= 1 = 0
  | x <= 0 = 0
  | otherwise = ceiling (negate (logBase 10 x)) - 1

mkPriceDeclaration :: Day -> CurrencySymbol -> RationalExpression () -> CurrencySymbol -> PriceDeclaration ()
mkPriceDeclaration day symbol rateExpression target =
  let priceDeclarationTimestamp = noLoc $ TimestampDay day
      priceDeclarationCurrencySymbol = noLoc symbol
      costExpressionConversionRate = noLoc rateExpression
      costExpressionCurrencySymbol = noLoc target
      priceDeclarationCost = noLoc $ CostExpression {..}
   in PriceDeclaration {..}
