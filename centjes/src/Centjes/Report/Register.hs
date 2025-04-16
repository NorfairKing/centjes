{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.Register
  ( Register (..),
    RegisterError (..),
    produceRegister,
  )
where

import Centjes.Convert
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Centjes.Filter (Filter)
import qualified Centjes.Filter as Filter
import Centjes.Ledger
import Centjes.Location
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.State
import Data.Time
import Data.Validity (Validity (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount

newtype Register ann = Register
  { registerTransactions ::
      Vector
        ( GenLocated ann Timestamp,
          Maybe (GenLocated ann Description),
          Vector
            ( GenLocated ann (Posting ann),
              Money.MultiAccount (Currency ann)
            )
        )
  }
  deriving (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (Register ann)

data RegisterError ann
  = RegisterErrorAddError
  | RegisterErrorConvertError !(ConvertError ann)
  deriving stock (Show, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (RegisterError ann)

instance ToReport (RegisterError SourceSpan) where
  toReport = \case
    RegisterErrorAddError -> undefined
    RegisterErrorConvertError ce -> toReport ce

unfoldrExactNMWithState ::
  forall a b m.
  (Monad m) =>
  Int ->
  (b -> m (a, b)) ->
  b ->
  m (Vector a, b)
unfoldrExactNMWithState n func start =
  runStateT (V.unfoldrExactNM n func' start) start
  where
    func' :: b -> StateT b m (a, b)
    func' b = do
      (a, b') <- lift $ func b
      put b'
      pure (a, b')

produceRegister ::
  forall ann.
  (Ord ann) =>
  Filter ->
  Maybe CurrencySymbol ->
  Bool ->
  Maybe Day ->
  Maybe Day ->
  Ledger ann ->
  Validation (RegisterError ann) (Register ann)
produceRegister f mCurrencySymbolTo showVirtual mBegin mEnd ledger = do
  mCurrencyTo <-
    mapValidationFailure RegisterErrorConvertError $
      traverse (lookupConversionCurrency (ledgerCurrencies ledger)) mCurrencySymbolTo

  ts <-
    V.catMaybes
      <$> traverse
        (registerTransaction f showVirtual)
        ( V.filter
            (transactionPassesDayFilter mBegin mEnd . locatedValue)
            (ledgerTransactions ledger)
        )
  let goTransaction ::
        ( Int,
          Money.MultiAccount (Currency ann),
          LatestPriceGraph (Currency ann),
          [GenLocated ann (Price ann)]
        ) ->
        Validation
          (RegisterError ann)
          ( ( GenLocated ann Timestamp,
              Maybe (GenLocated ann Description),
              Vector
                ( GenLocated ann (Posting ann),
                  Money.MultiAccount (Currency ann)
                )
            ),
            ( Int,
              Money.MultiAccount (Currency ann),
              LatestPriceGraph (Currency ann),
              [GenLocated ann (Price ann)]
            )
          )
      goTransaction (ix, runningTotal, priceGraph, prices) = do
        let (lts, ld, ps) = V.unsafeIndex ts ix

        -- Add all price declarations with timestamps before this transaction
        -- to the price graph.
        (priceGraph', newPrices) <- incorporatePricesUntil lts prices priceGraph

        let goPosting ::
              ( Int,
                Money.MultiAccount (Currency ann),
                LatestPriceGraph (Currency ann)
              ) ->
              Validation
                (RegisterError ann)
                ( ( GenLocated ann (Posting ann),
                    Money.MultiAccount (Currency ann)
                  ),
                  ( Int,
                    Money.MultiAccount (Currency ann),
                    LatestPriceGraph (Currency ann)
                  )
                )
            goPosting (jx, runningSubTotal, pg) = do
              let lp@(Located _ Posting {..}) = V.unsafeIndex ps jx
              let Located _ currency = postingCurrency
              let Located al account = postingAccount

              -- If there was a price, insert it into the price graph.
              let pg' = case postingCost of
                    Nothing -> pg
                    Just (Located _ Cost {..}) ->
                      let Located _ rate = costConversionRate
                          Located _ to = costCurrency
                          priority = Timestamp.toDay $ locatedValue lts
                       in PriceGraph.insert currency to rate priority pg

              newRunningSubTotal <-
                case MultiAccount.addAccount runningSubTotal currency account of
                  Nothing -> validationFailure RegisterErrorAddError
                  Just nt -> pure nt

              -- We need to re-convert every time because the price
              -- graph might have changed in this transaction, which
              -- means previous conversions are no longer accurate.
              --
              -- TODO consider having separate lines for this in the register report?
              newConvertedSubTotal <- case mCurrencyTo of
                Nothing -> pure newRunningSubTotal
                Just currencyTo -> do
                  let mpg = MemoisedPriceGraph.fromPriceGraph pg'
                  mapValidationFailure
                    RegisterErrorConvertError
                    ( convertMultiAccount
                        (Just al)
                        mpg
                        currencyTo
                        newRunningSubTotal
                    )

              pure
                ( (lp, newConvertedSubTotal),
                  ( succ jx,
                    newRunningSubTotal,
                    pg'
                  )
                )
        (newPostings, (_, newRunningTotal, newPriceGraph)) <-
          unfoldrExactNMWithState
            (V.length ps)
            goPosting
            (0, runningTotal, priceGraph')
        pure
          ( (lts, ld, newPostings),
            ( succ ix,
              newRunningTotal,
              newPriceGraph,
              newPrices
            )
          )

  ts' <-
    V.unfoldrExactNM
      (V.length ts)
      goTransaction
      ( 0,
        MultiAccount.zero,
        PriceGraph.empty,
        V.toList $ ledgerPrices ledger
      )

  pure $ Register ts'

incorporatePricesUntil ::
  (Ord ann) =>
  GenLocated ann Timestamp ->
  [GenLocated ann (Price ann)] ->
  LatestPriceGraph (Currency ann) ->
  Validation
    (RegisterError ann)
    ( LatestPriceGraph (Currency ann),
      [GenLocated ann (Price ann)]
    )
incorporatePricesUntil (Located _ timestamp) prices priceGraph =
  go priceGraph prices
  where
    go ::
      (Ord ann) =>
      LatestPriceGraph (Currency ann) ->
      [GenLocated ann (Price ann)] ->
      Validation
        (RegisterError ann)
        ( LatestPriceGraph (Currency ann),
          [GenLocated ann (Price ann)]
        )
    go pg = \case
      [] -> pure (pg, [])
      ps@((Located _ Price {..}) : restPrices) -> do
        let Located _ ts = priceTimestamp
        case Timestamp.comparePartially ts timestamp of
          Just LT -> do
            let Located _ from = priceCurrency
            let Located _ Cost {..} = priceCost
            let Located _ rate = costConversionRate
            let Located _ to = costCurrency
            let priority = Timestamp.toDay ts
            let pg' = PriceGraph.insert from to rate priority pg
            go pg' restPrices
          _ -> pure (pg, ps)

registerTransaction ::
  Filter ->
  Bool ->
  GenLocated ann (Transaction ann) ->
  Validation
    (RegisterError ann)
    ( Maybe
        ( GenLocated ann Timestamp,
          Maybe (GenLocated ann Description),
          Vector (GenLocated ann (Posting ann))
        )
    )
registerTransaction f showVirtual (Located _ t) = do
  postings <-
    V.catMaybes
      <$> traverse
        (registerPosting f showVirtual)
        (transactionPostings t)
  pure $
    if null postings
      then Nothing
      else
        Just
          ( transactionTimestamp t,
            transactionDescription t,
            postings
          )

transactionPassesDayFilter :: Maybe Day -> Maybe Day -> Transaction ann -> Bool
transactionPassesDayFilter mBegin mEnd t =
  let Located _ ts = transactionTimestamp t
   in and
        [ case mBegin of
            Nothing -> True
            Just begin -> Timestamp.toDay ts >= begin,
          case mEnd of
            Nothing -> True
            Just end -> Timestamp.toDay ts <= end
        ]

registerPosting ::
  Filter ->
  Bool ->
  GenLocated ann (Posting ann) ->
  Validation (RegisterError ann) (Maybe (GenLocated ann (Posting ann)))
registerPosting f showVirtual lp@(Located _ Posting {..}) = do
  let Located _ an = postingAccountName
      included = Filter.predicate f an
  pure $
    -- Don't show virtual postings by default
    if (postingReal || not postingReal && showVirtual) && included
      then Just lp
      else Nothing
