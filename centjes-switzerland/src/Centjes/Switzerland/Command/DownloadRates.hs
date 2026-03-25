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
import Centjes.Module (CostExpression (..), Declaration (..), Module (..), PriceDeclaration (..), RationalExpression (..), stripPriceDeclarationAnnotation)
import Centjes.Switzerland.OptParse
import Centjes.Timestamp (toDay)
import Centjes.Validation
import Conduit
import Control.Concurrent
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Conduit.Combinators as C
import Data.List (find, sortOn)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import qualified Data.Set as S
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

    -- Extract existing price declarations from the loaded declarations that came from the destination file
    let destinationRelFile = stripProperPrefix settingBaseDir downloadRatesSettingDestination
    let existingDeclarations =
          [ noLoc (DeclarationPrice (noLoc (stripPriceDeclarationAnnotation pd)))
          | Located loc (DeclarationPrice (Located _ pd)) <- declarations,
            Just relFile <- [destinationRelFile],
            sourceSpanFile loc == relFile
          ]

    -- Collect days that already have any rates in the output file
    let existingDays =
          S.fromList
            [ toDay (locatedValue (priceDeclarationTimestamp pd))
            | Located _ (DeclarationPrice (Located _ pd)) <- existingDeclarations
            ]

    newDeclarations <-
      runConduit $
        -- For each day
        yieldMany [downloadRatesSettingBegin .. downloadRatesSettingEnd]
          -- Skip days we already have rates for
          .| C.filter (not . (`S.member` existingDays))
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
          .| C.mapM
            ( \(day, request) -> do
                logDebugN $ T.pack $ unwords ["Fetching rates for", show day]
                response <- liftIO $ httpLbs request man
                let status = HTTP.statusCode (responseStatus response)
                if status /= 200
                  then do
                    logWarnN $ T.pack $ unwords ["HTTP request for", show day, "returned status", show status]
                    pure (day, Nothing)
                  else pure (day, Just response)
            )
          -- Filter out failed requests
          .| C.concatMap (\(day, mResponse) -> (,) day <$> mResponse)
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
          -- Skip rates that already exist in the file
          .| C.filter (\(day, _, _) -> not $ S.member day existingDays)
          -- Produce Price declarations
          .| C.map
            ( \(day, currencySymbol, rateExpression) ->
                let priceDeclarationTimestamp = noLoc $ TimestampDay day
                    priceDeclarationCurrencySymbol = noLoc currencySymbol
                    costExpressionConversionRate = noLoc rateExpression
                    costExpressionCurrencySymbol = noLoc $ CurrencySymbol "CHF"
                    priceDeclarationCost = noLoc $ CostExpression {..}
                 in noLoc $ DeclarationPrice $ noLoc PriceDeclaration {..}
            )
          .| C.sinkList

    let allDeclarations =
          sortOn priceDeclarationSortKey $
            existingDeclarations ++ newDeclarations

    liftIO $
      SB.writeFile (fromAbsFile downloadRatesSettingDestination) $
        TE.encodeUtf8 $
          formatModule $
            Module
              { moduleImports = [],
                moduleDeclarations = allDeclarations
              }

priceDeclarationSortKey :: GenLocated () (Declaration ()) -> (Day, CurrencySymbol)
priceDeclarationSortKey (Located _ (DeclarationPrice (Located _ pd))) =
  (toDay (locatedValue (priceDeclarationTimestamp pd)), locatedValue (priceDeclarationCurrencySymbol pd))
priceDeclarationSortKey _ = error "priceDeclarationSortKey: not a price declaration"
