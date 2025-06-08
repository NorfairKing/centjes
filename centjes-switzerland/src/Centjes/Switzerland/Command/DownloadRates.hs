{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Command.DownloadRates (runCentjesSwitzerlandDownloadRates) where

import Centjes.Compile
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Format (formatModule)
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Module (CostExpression (..), Declaration (..), Module (..), PriceDeclaration (..), RationalExpression (..))
import Centjes.Switzerland.OptParse
import Centjes.Validation
import Conduit
import Control.Concurrent
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Conduit.Combinators as C
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types as HTTP
import Numeric.DecimalLiteral as DecimalLiteral
import Path
import Text.XML as XML

runCentjesSwitzerlandDownloadRates :: Settings -> DownloadRatesSettings -> IO ()
runCentjesSwitzerlandDownloadRates Settings {..} DownloadRatesSettings {..} =
  runStderrLoggingT $ do
    -- Produce the input.json structure
    (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
    currencies <- liftIO $ checkValidation diag $ compileDeclarationsCurrencies declarations
    man <- liftIO newTlsManager
    generatedDeclarations <-
      runConduit $
        -- For each day
        yieldMany [downloadRatesSettingBegin .. downloadRatesSettingEnd]
          .| C.mapM
            ( \a -> do
                -- Gentle delay
                liftIO $ threadDelay 100_000
                pure a
            )
          -- Do a query to the rates endpoint
          .| C.concatMap
            ( \day -> do
                requestPrototype <- HTTP.parseRequest @Maybe "https://www.backend-rates.bazg.admin.ch/api/xmldaily"
                let params = [("d", Just $ TE.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%Y%m%d" day), ("locale", Just "en")]
                let request = HTTP.setQueryString params requestPrototype
                pure (day, request)
            )
          .| C.mapM (\(day, request) -> (,) day <$> liftIO (httpLbs request man))
          -- Ignore failed requests
          .| C.filter (\(_, response) -> HTTP.statusCode (responseStatus response) == 200)
          -- Parse XML from the response body
          .| C.concatMap (\(day, response) -> (,) day <$> XML.parseLBS XML.def (responseBody response))
          -- Get the individual elements out
          .| C.concatMap (\(day, document) -> (,) day <$> XML.elementNodes (XML.documentRoot document))
          -- Get the 'waehrung' and 'kurs' texts out
          .| C.concatMap
            ( \(day, node) -> do
                e <- case node of
                  NodeElement e -> pure e
                  _ -> Nothing
                let subElements =
                      mapMaybe
                        ( \case
                            NodeElement e' -> Just e'
                            _ -> Nothing
                        )
                        (elementNodes e)
                let lookupElement name = do
                      n <- find ((== name) . nameLocalName . elementName) subElements
                      case elementNodes n of
                        [NodeContent t] -> Just t
                        _ -> Nothing
                waehrung <- lookupElement "waehrung"
                kurs <- lookupElement "kurs"
                Just (day, waehrung, kurs)
            )
          -- Parse them into decimal literals and a currency symbol
          .| C.concatMap
            ( \(day, waehrungText, kursText) -> do
                (baseLitText, currencySymbolText) <- case T.splitOn " " waehrungText of
                  [baseLitText, currencySymbolText] -> pure (baseLitText, currencySymbolText)
                  _ -> Nothing
                baseLiteral <- DecimalLiteral.fromString $ T.unpack baseLitText
                currencySymbol <- CurrencySymbol.fromTextM currencySymbolText
                rateLiteral <- DecimalLiteral.fromString $ T.unpack kursText
                pure (day, baseLiteral, currencySymbol, rateLiteral)
            )
          -- Only continue with known currencies
          .| C.filter (\(_, _, currencySymbol, _) -> M.member currencySymbol currencies)
          -- Parse into an actual rate
          .| C.concatMap
            ( \(day, baseLiteral, currencySymbol, rateLiteral) -> do
                baseRatio <- DecimalLiteral.toRatio baseLiteral
                rateRatio <- DecimalLiteral.toRatio rateLiteral
                let actualRate = rateRatio / baseRatio
                let rateExpression = case DecimalLiteral.fromRatio actualRate of
                      Just r ->
                        RationalExpression
                          { rationalExpressionNumerator = noLoc r,
                            rationalExpressionDenominator = Nothing,
                            rationalExpressionPercent = False
                          }
                      Nothing ->
                        RationalExpression
                          { rationalExpressionNumerator = noLoc (DecimalLiteral.fromNatural (numerator actualRate)),
                            rationalExpressionDenominator = Just (noLoc (DecimalLiteral.fromNatural (denominator actualRate))),
                            rationalExpressionPercent = False
                          }
                pure (day, currencySymbol, rateExpression)
            )
          -- \| Produce Price declarations
          .| C.map
            ( \(day, currencySymbol, rateExpression) ->
                let priceDeclarationTimestamp = noLoc $ TimestampDay day
                    priceDeclarationCurrencySymbol = noLoc currencySymbol
                    costExpressionConversionRate = noLoc rateExpression
                    costExpressionCurrencySymbol = noLoc $ CurrencySymbol "CHF"
                    priceDeclarationCost = noLoc $ CostExpression {..}
                 in PriceDeclaration {..}
            )
          .| C.map (noLoc . DeclarationPrice . noLoc)
          .| C.sinkList
    liftIO $
      SB.writeFile (fromAbsFile downloadRatesSettingDestination) $
        TE.encodeUtf8 $
          formatModule $
            Module
              { moduleImports = [],
                moduleDeclarations = generatedDeclarations
              }
