{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Command.RatesGraph (runCentjesRatesGraph) where

import Centjes.Compile
import Centjes.Convert
import Centjes.Convert.PriceGraph as PriceGraph (Direction (..), PriceGraph (..))
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Load
import Centjes.OptParse
import Centjes.Timing
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.GraphViz
import Data.GraphViz.Attributes.HTML as HTML
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Monadic
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Money.ConversionRate as ConversionRate
import Path
import System.IO
import Text.Printf

runCentjesRatesGraph :: Settings -> RatesGraphSettings -> LoggingT IO ()
runCentjesRatesGraph Settings {..} RatesGraphSettings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) ->
    withLoggedDuration "RatestGraph" $ do
      let diagnostic = diagFromFileMap fileMap
      ledger <- withLoggedDuration "Compile" $ liftIO $ checkValidation diagnostic $ compileDeclarations declarations
      let priceGraph = pricesToPriceGraph (ledgerPrices ledger)
          dotGraph = digraph (Data.GraphViz.Types.Monadic.Str "currency-rates") $ do
            forM_ (M.keys (ledgerCurrencies ledger)) $ \currencySymbol ->
              node' (CurrencySymbol.toText currencySymbol)
            forM_ (M.toList (unPriceGraph priceGraph)) $ \(currencyFrom, toMap) ->
              forM_ (M.toList toMap) $ \(currencyTo, dir) ->
                case dir of
                  PriceGraph.Backward {} -> pure ()
                  PriceGraph.Forward (rate, priority) -> do
                    let from = currencySymbol currencyFrom
                        to = currencySymbol currencyTo
                    edge
                      (CurrencySymbol.toText from)
                      (CurrencySymbol.toText to)
                      [ toLabel $
                          Table $
                            HTable
                              { tableFontAttrs = Just [Face "Monospace"],
                                tableAttrs = [],
                                tableRows =
                                  [ Cells [LabelCell [] $ HTML.Text [HTML.Str $ LT.fromStrict $ T.pack $ show priority]],
                                    Cells [LabelCell [] $ HTML.Text [HTML.Str $ LT.fromStrict $ T.pack $ printf "%.2f" $ (realToFrac :: Rational -> Double) $ ConversionRate.toRational rate]]
                                  ]
                              }
                      ]
      liftIO $ case ratesGraphSettingOutputFile of
        Nothing -> hPutDot stdout dotGraph
        Just outputFile -> writeDotFile (toFilePath outputFile) dotGraph
