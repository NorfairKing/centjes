{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert
  ( ConvertError (..),
    lookupConversionCurrency,
    convertMultiAccount,
    convertMultiAccountToAccount,
    lookupConversionRate,
    pricesToMemoisedPriceGraph,
    pricesToPriceGraph,
    pricesToDailyPriceGraphs,
  )
where

import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Convert.PriceGraph (PriceGraph)
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import qualified Money.QuantisationFactor as Money (QuantisationFactor)

{-# ANN module ("DisableMutations" :: String) #-}

data ConvertError ann
  = ConvertErrorUnknownTarget !CurrencySymbol
  | -- | You can convert out of a lot, but never into one.
    ConvertErrorMissingPrice !(Maybe ann) !(Commodity ann) !(Currency ann)
  | ConvertErrorInvalidSum !(Currency ann)
  deriving (Show, Generic)

instance (Validity ann) => Validity (ConvertError ann)

instance ToReport (ConvertError SourceSpan) where
  toReport = \case
    -- [tag:CONVERT_ERROR_UNKNOWN_TARGET] At least one test per error:
    --   test_resources/balance/error/CONVERT_ERROR_UNKNOWN_TARGET.cent
    --   test_resources/register/error/CONVERT_ERROR_UNKNOWN_TARGET.cent
    ConvertErrorUnknownTarget cs ->
      Err
        (Just "CONVERT_ERROR_UNKNOWN_TARGET")
        ("Unknown currency to convert to: " <> CurrencySymbol.toString cs)
        []
        []
    -- [tag:CONVERT_ERROR_MISSING_PRICE] At least one test per error:
    --   test_resources/balance/error/CONVERT_ERROR_MISSING_PRICE.cent
    --   test_resources/register/error/CONVERT_ERROR_MISSING_PRICE.cent
    ConvertErrorMissingPrice mAl fromCommodity toCurrency ->
      let Located fromL _ = commodityQuantisationFactor fromCommodity
          Located toL _ = currencyQuantisationFactor toCurrency
       in Err
            (Just "CONVERT_ERROR_MISSING_PRICE")
            ( unwords
                [ "Could not convert an amount because the conversion rate from",
                  T.unpack (commodityText fromCommodity),
                  "to",
                  T.unpack (currencySymbolText (currencySymbol toCurrency)),
                  "cannot be determined"
                ]
            )
            ( concat
                [ [ (toDiagnosePosition fromL, Where "from this currency"),
                    (toDiagnosePosition toL, Where "to this currency")
                  ],
                  [ (toDiagnosePosition al, This "Failed to convert this amount")
                  | al <- maybeToList mAl
                  ]
                ]
            )
            []
    -- [tag:CONVERT_ERROR_INVALID_SUM] At least one test per error:
    --   test_resources/balance/error/CONVERT_ERROR_INVALID_SUM.cent
    --   test_resources/register/error/CONVERT_ERROR_INVALID_SUM.cent
    ConvertErrorInvalidSum currency
      | Located cl _ <- currencyQuantisationFactor currency ->
          Err
            (Just "CONVERT_ERROR_INVALID_SUM")
            "Could not sum converted amounts together because the result became too big."
            [ (toDiagnosePosition cl, Where "Trying to convert to this currency")
            ]
            []

lookupConversionCurrency ::
  Map CurrencySymbol (GenLocated ann Money.QuantisationFactor) ->
  CurrencySymbol ->
  Validation (ConvertError ann) (Currency ann)
lookupConversionCurrency currencies currencySymbolTo =
  case M.lookup currencySymbolTo currencies of
    Nothing -> validationFailure $ ConvertErrorUnknownTarget currencySymbolTo
    Just lqf -> pure $ Currency currencySymbolTo lqf

convertMultiAccount ::
  (Ord ann) =>
  Maybe ann ->
  MemoisedPriceGraph (Commodity ann) ->
  Currency ann ->
  Money.MultiAccount (Commodity ann) ->
  Validation (ConvertError ann) (Money.MultiAccount (Commodity ann))
convertMultiAccount al graph currencyTo ma =
  MultiAccount.fromAccount (CommodityCurrency currencyTo)
    <$> convertMultiAccountToAccount al graph currencyTo ma

convertMultiAccountToAccount ::
  (Ord ann) =>
  Maybe ann ->
  MemoisedPriceGraph (Commodity ann) ->
  Currency ann ->
  Money.MultiAccount (Commodity ann) ->
  Validation (ConvertError ann) Money.Account
convertMultiAccountToAccount al graph currencyTo ma = do
  let quantisationFactorTo :: Money.QuantisationFactor
      quantisationFactorTo = locatedValue (currencyQuantisationFactor currencyTo)
  (mResult, _) <-
    MultiAccount.convertAllA
      MultiAccount.RoundNearest
      quantisationFactorTo
      (lookupConversionRate al graph currencyTo)
      ma
  case mResult of
    Nothing -> validationFailure $ ConvertErrorInvalidSum currencyTo
    Just result -> pure result

lookupConversionRate ::
  forall ann.
  (Ord ann) =>
  Maybe ann ->
  MemoisedPriceGraph (Commodity ann) ->
  Currency ann ->
  Commodity ann ->
  Validation (ConvertError ann) (Money.ConversionRate, Money.QuantisationFactor)
lookupConversionRate al graph currencyTo commodityFrom = do
  case MemoisedPriceGraph.lookup graph commodityFrom (CommodityCurrency currencyTo) of
    Nothing -> validationFailure $ ConvertErrorMissingPrice al commodityFrom currencyTo
    Just rate -> pure (rate, locatedValue (commodityQuantisationFactor commodityFrom))

pricesToMemoisedPriceGraph ::
  (Ord ann) =>
  Vector (GenLocated ann (Price ann)) ->
  MemoisedPriceGraph (Commodity ann)
pricesToMemoisedPriceGraph = MemoisedPriceGraph.fromPriceGraph . pricesToPriceGraph

pricesToPriceGraph ::
  (Ord ann) =>
  Vector (GenLocated ann (Price ann)) ->
  PriceGraph Day (Commodity ann)
pricesToPriceGraph = V.foldl go PriceGraph.empty
  where
    go g (Located _ Price {..}) =
      let Located _ commodityFrom = priceCommodity
          Located _ Cost {..} = priceCost
          Located _ rate = costConversionRate
          Located _ currencyTo = costCurrency
          Located _ timestamp = priceTimestamp
          priority = Timestamp.toDay timestamp
       in PriceGraph.insert commodityFrom (CommodityCurrency currencyTo) rate priority g

pricesToDailyPriceGraphs ::
  (Ord ann) =>
  Vector (GenLocated ann (Price ann)) ->
  Map Day (MemoisedPriceGraph (Commodity ann))
pricesToDailyPriceGraphs = M.map MemoisedPriceGraph.fromPriceGraph . fst . V.foldl go (M.empty, PriceGraph.empty)
  where
    go (m, pg) (Located _ Price {..}) =
      let Located _ commodityFrom = priceCommodity
          Located _ Cost {..} = priceCost
          Located _ rate = costConversionRate
          Located _ currencyTo = costCurrency
          Located _ timestamp = priceTimestamp
          priority = Timestamp.toDay timestamp
          pg' = PriceGraph.insert commodityFrom (CommodityCurrency currencyTo) rate priority pg
          m' = M.insert (Timestamp.toDay timestamp) pg' m
       in (m', pg')
