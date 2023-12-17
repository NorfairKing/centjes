{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Ledger
  ( Ledger (..),
    Timestamp (..),
    CurrencySymbol (..),
    Price (..),
    Cost (..),
    Transaction (..),
    Description (..),
    Posting (..),
    Assertion (..),
    Currency (..),
    AccountName (..),
  )
where

import Centjes.AccountName (AccountName (..))
import Centjes.Location
import Centjes.Module (CurrencySymbol (..), Description (..))
import Centjes.Timestamp as Timestamp
import Control.DeepSeq
import Data.Function
import Data.Map.Strict (Map)
import Data.Validity
import Data.Validity.Map ()
import Data.Validity.Vector ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Money.ConversionRate (ConversionRate)
import Money.QuantisationFactor

data Ledger ann = Ledger
  { ledgerCurrencies :: !(Map CurrencySymbol (GenLocated ann QuantisationFactor)),
    ledgerPrices :: !(Vector (GenLocated ann (Price ann))),
    ledgerTransactions :: !(Vector (GenLocated ann (Transaction ann)))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Ledger ann) where
  validate l@(Ledger {..}) =
    mconcat
      [ genericValidate l,
        -- TODO all the currencies are consistent
        declare "the prices are sorted" $
          partiallyOrderedBy
            (Timestamp.comparePartially `on` locatedValue . priceTimestamp . locatedValue)
            ledgerPrices,
        declare "the transactions are sorted" $
          partiallyOrderedBy
            (Timestamp.comparePartially `on` locatedValue . transactionTimestamp . locatedValue)
            ledgerTransactions
      ]

instance NFData ann => NFData (Ledger ann)

partiallyOrderedBy :: (a -> a -> Maybe Ordering) -> Vector a -> Bool
partiallyOrderedBy f v =
  if V.null v
    then True
    else V.and (V.zipWith (\a1 a2 -> f a1 a2 /= Just GT) v (V.tail v))

data Price ann = Price
  { priceTimestamp :: !(GenLocated ann Timestamp),
    -- Note: This field will have the source location of the currency _symbol_ in the price declaration.
    priceCurrency :: !(GenLocated ann (Currency ann)),
    -- Note: This field will have the source declaration of the cost wherever it was declared.
    priceCost :: !(GenLocated ann (Cost ann))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Price ann)

instance NFData ann => NFData (Price ann)

data Cost ann = Cost
  { -- Note: This field will have the source location of the decimal literal in the cost
    costConversionRate :: !(GenLocated ann ConversionRate),
    -- Note: This field will have the source location of the currency _symbol_ in the cost
    costCurrency :: !(GenLocated ann (Currency ann))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Cost ann)

instance NFData ann => NFData (Cost ann)

data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: !(Vector (GenLocated ann (Posting ann))),
    transactionAssertions :: !(Vector (GenLocated ann (Assertion ann)))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Transaction ann)

instance NFData ann => NFData (Transaction ann)

data Posting ann = Posting
  { postingAccountName :: !(GenLocated ann AccountName),
    -- Note: This field will have the source location of the currency _symbol_ that defined it.
    postingCurrency :: !(GenLocated ann (Currency ann)),
    -- Note: This field will have the source location of the decimal literal that defined it.
    postingAccount :: !(GenLocated ann Money.Account),
    postingCost :: !(Maybe (GenLocated ann (Cost ann)))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Posting ann)

instance NFData ann => NFData (Posting ann)

data Assertion ann
  = AssertionEquals
      !(GenLocated ann AccountName)
      -- Note: This field will have the source location of the decimal literal that defined it.
      !(GenLocated ann Money.Account)
      -- Note: This field will have the source location of the currency _symbol_ that defined it
      !(GenLocated ann (Currency ann))
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Assertion ann)

instance NFData ann => NFData (Assertion ann)

data Currency ann = Currency
  { currencySymbol :: !CurrencySymbol,
    -- Note: This field will have the source location of currency _declaration_ that defined it
    currencyQuantisationFactor :: !(GenLocated ann QuantisationFactor)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ann => Validity (Currency ann)

instance NFData ann => NFData (Currency ann)
