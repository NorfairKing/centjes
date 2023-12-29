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
    lookupConversionRate,
  )
where

import Centjes.Convert.PriceGraph (PriceGraph)
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Location
import Centjes.Validation
import Control.DeepSeq
import qualified Data.Map as M
import Data.Map.Strict (Map)
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic)
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import qualified Money.QuantisationFactor as Money (QuantisationFactor)

data ConvertError ann
  = ConvertErrorUnknownTarget !CurrencySymbol
  | ConvertErrorMissingPrice !(Currency ann) !(Currency ann) !ann
  | ConvertErrorInvalidSum !(Currency ann) !ann
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (ConvertError ann)

instance NFData ann => NFData (ConvertError ann)

instance ToReport (ConvertError SourceSpan) where
  toReport = \case
    ConvertErrorUnknownTarget cs ->
      Err
        (Just "CONVERT_ERROR_UNKNOWN_TARGET")
        ("Unknown currency to convert to: " <> CurrencySymbol.toString cs)
        []
        []
    ConvertErrorMissingPrice (Currency fromSymbol (Located fromL _)) (Currency toSymbol (Located toL _)) tl ->
      Err
        (Just "CONVERT_ERROR_MISSING_PRICE")
        ( unwords
            [ "Could not convert an amount because the conversion rate from",
              CurrencySymbol.toString fromSymbol,
              "to",
              CurrencySymbol.toString toSymbol,
              "cannot be determined"
            ]
        )
        [ (toDiagnosePosition fromL, Where "Trying to convert from this currency"),
          (toDiagnosePosition toL, Where "Trying to convert to this currency"),
          (toDiagnosePosition tl, Where "Trying to convert these amounts")
        ]
        []
    ConvertErrorInvalidSum (Currency _ (Located cl _)) tl ->
      Err
        (Just "CONVERT_ERROR_INVALID_SUM")
        "Could not sum converted amounts together because the result became too big."
        [ (toDiagnosePosition cl, Where "Trying to convert to this currency"),
          (toDiagnosePosition tl, Where "Trying to convert these amounts")
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
  Ord ann =>
  Vector (GenLocated ann (Price ann)) ->
  Currency ann ->
  ann ->
  Money.MultiAccount (Currency ann) ->
  Validation (ConvertError ann) (Money.MultiAccount (Currency ann))
convertMultiAccount prices currencyTo l ma = do
  let quantisationFactorTo :: Money.QuantisationFactor
      quantisationFactorTo = locatedValue (currencyQuantisationFactor currencyTo)
  (mResult, _) <-
    MultiAccount.convertAllA
      MultiAccount.RoundNearest
      quantisationFactorTo
      (lookupConversionRate prices currencyTo l)
      ma
  case mResult of
    Nothing -> validationFailure $ ConvertErrorInvalidSum currencyTo l
    Just result -> pure $ MultiAccount.fromAccount currencyTo result

lookupConversionRate ::
  forall ann.
  Ord ann =>
  Vector (GenLocated ann (Price ann)) ->
  Currency ann ->
  ann ->
  Currency ann ->
  Validation (ConvertError ann) (Money.ConversionRate, Money.QuantisationFactor)
lookupConversionRate prices currencyTo l currencyFrom = do
  let graph = toPriceGraph prices
  case PriceGraph.lookup graph currencyFrom currencyTo of
    Nothing -> validationFailure $ ConvertErrorMissingPrice currencyTo currencyFrom l
    Just rate -> pure (rate, locatedValue (currencyQuantisationFactor currencyFrom))

-- TODO speed this up by not recomputing this entire graph for every conversion
toPriceGraph ::
  Ord ann =>
  Vector (GenLocated ann (Price ann)) ->
  PriceGraph (Currency ann)
toPriceGraph = V.foldl go PriceGraph.empty
  where
    go g (Located _ Price {..}) =
      let Located _ currencyFrom = priceCurrency
          Located _ Cost {..} = priceCost
          Located _ rate = costConversionRate
          Located _ currencyTo = costCurrency
       in PriceGraph.insert currencyFrom currencyTo rate g
