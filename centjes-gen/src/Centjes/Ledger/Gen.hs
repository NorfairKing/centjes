{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Ledger.Gen where

import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Location.Gen
import Centjes.Module.Gen ()
import Centjes.Timestamp as Timestamp
import Data.Function
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Set ()
import Data.GenValidity.Vector ()
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Vector as V
import Money.Gen ()
import Money.QuantisationFactor (QuantisationFactor)
import Numeric.DecimalLiteral.Gen ()
import Test.QuickCheck

instance (Ord ann, GenValid ann) => GenValid (Ledger ann) where
  genValid = do
    ledgerCurrencies <- genValid
    ledgerAccounts <- genValid -- TODO deduce the accounts from the other fields
    ledgerTags <- genValid -- TODO deduce the tags from the other fields
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
            <$> genListOf (genLocatedWith (genPriceWithCurrencies ledgerCurrencies))
    ledgerTransactions <-
      if null ledgerCurrencies
        then pure V.empty
        else
          V.fromList
            . sortBy
              ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
                  `on` locatedValue
                    . Ledger.transactionTimestamp
                    . locatedValue
              )
            <$> genListOf (genLocatedWith (genTransactionWithCurrencies ledgerCurrencies))
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

instance (Ord ann, GenValid ann) => GenValid (Transaction ann)

-- Map must not be empty
genTransactionWithCurrencies ::
  GenValid ann =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Transaction ann)
genTransactionWithCurrencies currencies = do
  transactionTimestamp <- genValid
  transactionDescription <- genValid
  transactionPostings <- V.fromList <$> genListOf (genLocatedWith (genPostingWithCurrencies currencies))
  transactionAttachments <- genValid
  transactionAssertions <- V.fromList <$> genListOf (genLocatedWith (genAssertionWithCurrencies currencies))
  transactionTags <- genValid
  pure Transaction {..}

instance GenValid ann => GenValid (Posting ann)

-- Map must not be empty
genPostingWithCurrencies ::
  GenValid ann =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Posting ann)
genPostingWithCurrencies currencies = do
  postingReal <- genValid
  postingAccountName <- genValid
  postingCurrency <- genLocatedWith $ chooseCurrency currencies
  postingAccount <- genValid
  postingCost <-
    frequency
      [ (1, pure Nothing),
        (3, fmap Just $ genLocatedWith $ genCostWithCurrencies currencies)
      ]
  postingPercentage <- genValid
  pure Posting {..}

instance GenValid ann => GenValid (Assertion ann)

-- Map must not be empty
genAssertionWithCurrencies ::
  GenValid ann =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Assertion ann)
genAssertionWithCurrencies currencies = do
  an <- genValid
  acc <- genValid
  cur <- genLocatedWith $ chooseCurrency currencies
  pure $ AssertionEquals an acc cur

instance GenValid ann => GenValid (Price ann)

-- Map must not be empty
genPriceWithCurrencies ::
  GenValid ann =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Price ann)
genPriceWithCurrencies currencies = do
  priceTimestamp <- genValid
  priceCurrency <- genLocatedWith $ chooseCurrency currencies
  priceCost <- genLocatedWith $ genCostWithCurrencies currencies
  pure Price {..}

instance GenValid ann => GenValid (Cost ann)

-- Map must not be empty
genCostWithCurrencies ::
  GenValid ann =>
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Cost ann)
genCostWithCurrencies currencies = do
  costConversionRate <- genValid
  costCurrency <- genLocatedWith $ chooseCurrency currencies
  pure Cost {..}

instance GenValid ann => GenValid (Percentage ann)

instance GenValid ann => GenValid (Currency ann)

-- Map must not be empty
chooseCurrency ::
  Map CurrencySymbol (GenLocated ann QuantisationFactor) ->
  Gen (Currency ann)
chooseCurrency =
  fmap (uncurry Currency) . elements . M.toList
