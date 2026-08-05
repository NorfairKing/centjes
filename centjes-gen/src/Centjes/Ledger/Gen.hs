{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Ledger.Gen where

import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Location.Gen
import Centjes.Module.Gen ()
import Centjes.Timestamp as Timestamp
import Control.Monad
import Data.Function
import Data.GenValidity
import Data.GenValidity.Map
import Data.GenValidity.Set ()
import Data.GenValidity.Vector ()
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import Money.Gen ()
import Money.QuantisationFactor (QuantisationFactor)
import Numeric.DecimalLiteral.Gen ()
import Test.QuickCheck

instance (Ord ann, GenValid ann) => GenValid (Ledger ann) where
  genValid = do
    ledgerCurrencies <- genValid
    ledgerAccounts <- genValid
    ledgerTags <- genValid
    ledgerPrices <-
      if null ledgerCurrencies
        then pure V.empty
        else
          V.fromList
            . sortBy
              ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
                  `on` locatedValue
                    . Ledger.priceTimestamp
                    . locatedValue
              )
            . catMaybes
            <$> genListOf (genMLocatedWith (genPriceWithCurrencies ledgerCurrencies))
    ledgerTransactions <-
      V.fromList
        . sortBy
          ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
              `on` locatedValue
                . Ledger.transactionTimestamp
                . locatedValue
          )
        <$> genListOf (genLocatedWith (genTransactionWith ledgerAccounts ledgerCurrencies ledgerTags))
    pure Ledger {..}
  shrinkValid l =
    filter isValid
      . filter (/= l)
      . map
        ( \l' ->
            l'
              { ledgerPrices =
                  V.fromList $
                    sortBy
                      ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
                          `on` locatedValue
                            . Ledger.priceTimestamp
                            . locatedValue
                      )
                      (V.toList (ledgerPrices l')),
                ledgerTransactions =
                  V.fromList $
                    sortBy
                      ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
                          `on` locatedValue
                            . Ledger.transactionTimestamp
                            . locatedValue
                      )
                      (V.toList (ledgerTransactions l'))
              }
        )
      $ shrinkValidStructurallyWithoutExtraFiltering l

instance (Ord ann, GenValid ann) => GenValid (Account ann)

instance GenValid VirtualPostingPolicy

instance (Ord ann, GenValid ann) => GenValid (Transaction ann)

-- Map must not be empty
genTransactionWith ::
  (GenValid ann) =>
  Map AccountName (GenLocated ann (Account ann)) ->
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Map Tag ann ->
  Gen (Transaction ann)
genTransactionWith accounts currencies tags = do
  transactionTimestamp <- genValid
  transactionDescription <- genValid
  transactionPostings <-
    if null accounts || null currencies
      then pure V.empty
      else V.fromList <$> genListOf (genLocatedWith (genPostingWith accounts currencies))
  transactionAttachments <- genValid
  transactionAssertions <-
    if null accounts || null currencies
      then pure V.empty
      else V.fromList <$> genListOf (genLocatedWith (genAssertionWith accounts currencies))
  transactionTags <-
    if null tags
      then pure M.empty
      else genMapOf ((,) <$> chooseTag tags <*> genValid)
  pure Transaction {..}

instance (Eq ann, GenValid ann) => GenValid (Posting ann)

instance (Eq ann, GenValid ann) => GenValid (PostingPrice ann)

-- Map must not be empty
genPostingWith ::
  (GenValid ann) =>
  Map AccountName (GenLocated ann (Account ann)) ->
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Posting ann)
genPostingWith accounts currencies = do
  postingReal <- genValid
  postingAccountName <- genLocatedWith $ chooseAccountName accounts
  declared <- genLocatedWith $ chooseCurrency currencies
  mLot <- genMLotWithCurrencies currencies (locatedValue declared)
  postingPrice <- case mLot of
    Just lot -> pure $ PostingPriceLot (lot <$ declared)
    Nothing -> do
      mCost <-
        frequency
          [ (1, pure Nothing),
            (3, genMLocatedWith $ genCostWithCurrencies currencies declared)
          ]
      pure $ PostingPriceCurrency declared mCost
  postingAccount <- genValid
  postingAmountRatio <- genValid
  pure Posting {..}

instance (GenValid ann) => GenValid (Assertion ann)

-- Map must not be empty
genAssertionWith ::
  (GenValid ann) =>
  Map AccountName (GenLocated ann (Account ann)) ->
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Assertion ann)
genAssertionWith accounts currencies = do
  an <- genLocatedWith $ chooseAccountName accounts
  acc <- genValid
  cur <- genLocatedWith $ chooseCommodity currencies
  scope <- genValid
  pure $ AssertionEquals scope an acc cur

instance (Eq ann, GenValid ann) => GenValid (Price ann)

-- Map must not be empty
genPriceWithCurrencies ::
  (GenValid ann) =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Maybe (Price ann))
genPriceWithCurrencies currencies = do
  priceCommodity <- genLocatedWith $ chooseCommodity currencies
  mCost <- genMLocatedWith $ genCostWithCurrencies currencies (commodityCurrency <$> priceCommodity)
  forM mCost $ \priceCost -> do
    priceTimestamp <- genValid
    pure Price {..}

instance (GenValid ann) => GenValid (Cost ann)

-- Map must not be empty
genCostWithCurrencies ::
  (GenValid ann) =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  GenLocated ann (Currency ann) ->
  Gen (Maybe (Cost ann))
genCostWithCurrencies currencies (Located _ referenceCurrency) = do
  costConversionRate <- genValid
  let symbol = currencySymbol referenceCurrency
  mCurrency <- genMLocatedWith $ chooseMCurrency $ M.delete symbol currencies
  forM mCurrency $ \costCurrency ->
    pure Cost {..}

instance (GenValid ann) => GenValid (AmountRatio ann)

instance (GenValid ann) => GenValid (Commodity ann)

instance (GenValid ann) => GenValid (Currency ann)

instance (GenValid ann) => GenValid (Lot ann)

-- Map must not be empty
chooseAccountName ::
  Map AccountName val ->
  Gen AccountName
chooseAccountName =
  elements . S.toList . M.keysSet

-- Only Nothing if there are no currencies
chooseMCurrency ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Maybe (Currency ann))
chooseMCurrency currencies =
  if null currencies
    then pure Nothing
    else Just <$> chooseCurrency currencies

-- Map must not be empty
chooseCurrency ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Currency ann)
chooseCurrency =
  fmap (uncurry Currency) . elements . M.toList

-- Map must not be empty
chooseCommodity ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Commodity ann)
chooseCommodity currencies = do
  currency <- chooseCurrency currencies
  mLot <- genMLotWithCurrencies currencies currency
  pure $ maybe (CommodityCurrency currency) CommodityLot mLot

-- | Generate a lot in one of the other currencies, if there is one.
--
-- The basis currency has to come out of the currencies map, or the ledger it
-- ends up in is invalid.
genMLotWithCurrencies ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Currency ann ->
  Gen (Maybe (Lot ann))
genMLotWithCurrencies currencies lotCurrency =
  case M.toList (M.delete (currencySymbol lotCurrency) currencies) of
    [] -> pure Nothing
    others ->
      frequency
        [ (3, pure Nothing),
          ( 1,
            do
              lotBasisRate <- genValid
              lotBasisCurrency <- uncurry Currency <$> elements others
              pure $ Just Lot {..}
          )
        ]

-- Map must not be empty
chooseTag :: Map Tag ann -> Gen Tag
chooseTag =
  elements . S.toList . M.keysSet
