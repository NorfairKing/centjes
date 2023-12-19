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
import qualified Money.ConversionRate as ConversionRate
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
  Vector (GenLocated ann (Price ann)) ->
  Currency ann ->
  ann ->
  Currency ann ->
  Validation (ConvertError ann) (Money.ConversionRate, Money.QuantisationFactor)
lookupConversionRate prices currencyTo l currencyFrom =
  if currencySymbol currencyFrom == currencySymbol currencyTo
    then pure (ConversionRate.oneToOne, quantisationFactorTo)
    else case lastMatch (matchingPrice . locatedValue) prices of
      Nothing ->
        -- Could not convert because we don't have the price info.
        validationFailure $ ConvertErrorMissingPrice currencyTo currencyFrom l
      Just rate ->
        pure (rate, locatedValue (currencyQuantisationFactor currencyFrom))
  where
    quantisationFactorTo :: Money.QuantisationFactor
    quantisationFactorTo = locatedValue (currencyQuantisationFactor currencyTo)

    -- TODO this could probably be much faster instead of a linear search.
    matchingPrice :: Price ann -> Maybe Money.ConversionRate
    matchingPrice Price {..} =
      let new = locatedValue priceCurrency
          cost = locatedValue priceCost
          old = locatedValue (costCurrency cost)
          cr = locatedValue (costConversionRate cost)
       in if currencySymbol old == currencySymbol currencyTo
            && currencySymbol new == currencySymbol currencyFrom
            then Just cr
            else
              if currencySymbol old == currencySymbol currencyFrom
                && currencySymbol new == currencySymbol currencyTo
                then Just (ConversionRate.invert cr)
                else Nothing -- TODO also more complicated paths.

lastMatch :: (a -> Maybe b) -> Vector a -> Maybe b
lastMatch f v = go (V.length v - 1)
  where
    go ix = case v V.!? ix of
      Nothing -> Nothing
      Just a -> case f a of
        Nothing -> go (pred ix)
        Just b -> Just b
