{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Centjes.Cryptocurrencies.Command.DownloadRates (runCentjesCryptocurrenciesDownloadRates) where

import Centjes.Compile
import Centjes.Cryptocurrencies.OptParse
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format (formatModule)
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Validation
import Conduit
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Monad (guard)
import Control.Monad.Logger
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Conduit.Combinators as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Path
import Text.Printf (printf)

runCentjesCryptocurrenciesDownloadRates :: Settings -> DownloadRatesSettings -> IO ()
runCentjesCryptocurrenciesDownloadRates Settings {..} DownloadRatesSettings {..} = runStderrLoggingT $ do
  -- Load ledger to get declared currencies
  (declarations, diag) <- loadModules settingLedgerFile
  currencies <- liftIO $ checkValidation diag $ compileDeclarationsCurrencies declarations

  man <- liftIO newTlsManager

  -- Use provided symbols or fall back to all currencies from the ledger (excluding target)
  let symbols = maybe (filter (/= downloadRatesSettingTarget) $ M.keys currencies) NE.toList downloadRatesSettingSymbols

  generatedDeclarations <-
    runConduit $
      yieldMany [downloadRatesSettingBegin .. downloadRatesSettingEnd]
        -- Gentle delay between requests
        .| C.mapM (\day -> liftIO (threadDelay 100_000) >> pure day)
        -- Fetch USD rates for this day (one request per day)
        .| C.mapM (\day -> (day,) <$> fetchUsdRates man day)
        -- Filter out failed requests
        .| C.concatMap (\(day, mRates) -> (day,) <$> mRates)
        -- Expand to (day, rates, symbol) for each symbol we want
        .| C.concatMap (\(day, rates) -> (day,rates,) <$> symbols)
        -- Convert symbol to lowercase text for API lookup
        .| C.map (\(day, rates, symbol) -> (day, rates, symbol, T.toLower $ CurrencySymbol.toText symbol))
        -- Look up the rate for this symbol
        .| C.concatMap (\(day, rates, symbol, symbolTextLower) -> (day,symbol,) <$> KM.lookup (Key.fromText symbolTextLower) rates)
        -- Extract the numeric rate
        .| C.concatMap (\(day, symbol, val) -> (day,symbol,) <$> extractNumber val)
        -- Calculate the rate expression (invert: USD per 1 symbol)
        .| C.concatMap (\(day, symbol, symbolUsdRate) -> (day,symbol,) <$> calculateRate symbolUsdRate)
        -- Only continue with known currencies that are also cryptocurrencies
        .| C.filter (\(_, symbol, _) -> M.member symbol currencies)
        .| C.filter (\(_, symbol, _) -> isCryptocurrency symbol)
        -- Produce price declarations
        .| C.map (\(day, symbol, rateExpression) -> noLoc $ DeclarationPrice $ noLoc $ mkPriceDeclaration day symbol rateExpression downloadRatesSettingTarget)
        .| C.sinkList

  let output =
        TE.encodeUtf8 $
          formatModule $
            Module
              { moduleImports = [],
                moduleDeclarations = generatedDeclarations
              }

  liftIO $ case downloadRatesSettingOutput of
    Nothing -> SB.putStr output
    Just outputFile -> SB.writeFile (fromAbsFile outputFile) output

-- | Calculate rate: how many USD per 1 symbol unit
-- symbolUsdRate is how many of symbol you get for 1 USD
-- So: 1 symbol = (1 / symbolUsdRate) USD
calculateRate :: Scientific -> Maybe (RationalExpression ())
calculateRate symbolUsdRate = do
  guard (symbolUsdRate > 0)
  -- Convert to Double, invert, and format with appropriate decimals
  let invertedRate = 1 / toRealFloat symbolUsdRate :: Double
  -- Use 2 decimals for rates >= 1
  -- For rates < 1, use 4 significant digits after the leading zeros
  let decimals
        | invertedRate >= 1 = 2
        | otherwise = countLeadingZeroDecimals invertedRate + 4
  rateLiteral <- DecimalLiteral.fromString $ printf "%.*f" decimals invertedRate
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

extractNumber :: Value -> Maybe Scientific
extractNumber (Number n) = Just n
extractNumber _ = Nothing

fetchUsdRates :: Manager -> Day -> LoggingT IO (Maybe (KM.KeyMap Value))
fetchUsdRates man day = do
  let dateStr = formatTime defaultTimeLocale "%F" day
  let primaryUrl = "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@" <> dateStr <> "/v1/currencies/usd.json"
  let fallbackUrl = "https://" <> dateStr <> ".currency-api.pages.dev/v1/currencies/usd.json"

  tryFetchRates man primaryUrl >>= \case
    Just rates -> pure $ Just rates
    Nothing -> tryFetchRates man fallbackUrl

tryFetchRates :: Manager -> String -> LoggingT IO (Maybe (KM.KeyMap Value))
tryFetchRates man url = do
  logDebugN $ "Fetching " <> T.pack url
  request <- liftIO $ HTTP.parseRequest url
  result <- liftIO $ try $ HTTP.httpLbs request man
  case result of
    Left (e :: HTTP.HttpException) -> do
      logWarnN $ "HTTP request failed: " <> T.pack (show e)
      pure Nothing
    Right response ->
      if HTTP.statusCode (responseStatus response) /= 200
        then do
          logWarnN $ "HTTP request returned status " <> T.pack (show (HTTP.statusCode (responseStatus response)))
          pure Nothing
        else pure $ parseRatesResponse (responseBody response)

parseRatesResponse :: LB.ByteString -> Maybe (KM.KeyMap Value)
parseRatesResponse body = do
  Object obj <- decode body
  -- The JSON structure is {"date": "...", "usd": {"btc": rate, "chf": rate, ...}}
  Object usdRates <- KM.lookup "usd" obj
  pure usdRates

mkPriceDeclaration :: Day -> CurrencySymbol -> RationalExpression () -> CurrencySymbol -> PriceDeclaration ()
mkPriceDeclaration day symbol rateExpression target =
  let priceDeclarationTimestamp = noLoc $ TimestampDay day
      priceDeclarationCurrencySymbol = noLoc symbol
      costExpressionConversionRate = noLoc rateExpression
      costExpressionCurrencySymbol = noLoc target
      priceDeclarationCost = noLoc $ CostExpression {..}
   in PriceDeclaration {..}

-- | Check if a currency symbol is a known cryptocurrency
isCryptocurrency :: CurrencySymbol -> Bool
isCryptocurrency symbol = S.member (T.toUpper $ CurrencySymbol.toText symbol) knownCryptocurrencies

-- | Set of known cryptocurrency symbols (uppercase)
knownCryptocurrencies :: S.Set T.Text
knownCryptocurrencies =
  S.fromList
    [ "1INCH",
      "AAVE",
      "ADA",
      "ALGO",
      "APE",
      "APT",
      "ARB",
      "ATOM",
      "AVAX",
      "AXS",
      "BAT",
      "BCH",
      "BNB",
      "BTC",
      "BUSD",
      "CAKE",
      "COMP",
      "CRO",
      "CRV",
      "DAI",
      "DOGE",
      "DOT",
      "EGLD",
      "ENJ",
      "ENS",
      "EOS",
      "ETC",
      "ETH",
      "FIL",
      "FLOW",
      "FTM",
      "GALA",
      "GMT",
      "GRT",
      "HBAR",
      "HNT",
      "ICP",
      "IMX",
      "INJ",
      "JASMY",
      "KAVA",
      "KCS",
      "KLAY",
      "KSM",
      "LDO",
      "LEO",
      "LINK",
      "LRC",
      "LTC",
      "LUNA",
      "MANA",
      "MATIC",
      "MINA",
      "MKR",
      "NEAR",
      "NEO",
      "NEXO",
      "OP",
      "PAXG",
      "QNT",
      "RNDR",
      "RUNE",
      "SAND",
      "SHIB",
      "SNX",
      "SOL",
      "STX",
      "SUI",
      "SUSHI",
      "THETA",
      "TRX",
      "TUSD",
      "UNI",
      "USDC",
      "USDD",
      "USDP",
      "USDT",
      "VET",
      "WBTC",
      "WETH",
      "XLM",
      "XMR",
      "XRP",
      "XTZ",
      "YFI",
      "ZEC",
      "ZIL"
    ]
