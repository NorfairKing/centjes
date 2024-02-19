{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Ledger
  ( Ledger (..),
    Timestamp (..),
    CurrencySymbol (..),
    Price (..),
    Transaction (..),
    Assertion (..),
    Description (..),
    Posting (..),
    Cost (..),
    Percentage (..),
    Attachment (..),
    Tag (..),
    Currency (..),
    AccountName (..),
  )
where

import Centjes.AccountName (AccountName (..))
import Centjes.AccountType (AccountType (..))
import Centjes.Location
import Centjes.Module (Attachment (..), CurrencySymbol (..), Description (..))
import Centjes.Tag
import Centjes.Timestamp as Timestamp
import Control.DeepSeq
import Data.Function
import Data.Map.Strict (Map)
import Data.Ratio
import Data.Validity
import Data.Validity.Map ()
import Data.Validity.Set ()
import Data.Validity.Vector ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Money.ConversionRate (ConversionRate)
import Money.QuantisationFactor
import Numeric.Natural

data Ledger ann = Ledger
  { -- Note: This field will have the source location of the currency _declaration_ that defined it.
    ledgerCurrencies :: !(Map CurrencySymbol (GenLocated ann QuantisationFactor)),
    -- Note: This field will have the source location of the account _ declaration_ that defined it.
    ledgerAccounts :: !(Map AccountName (GenLocated ann AccountType)),
    -- Note: This field will have the source location of the tag _declaration_ that defined it
    ledgerTags :: !(Map Tag ann),
    ledgerPrices :: !(Vector (GenLocated ann (Price ann))),
    ledgerTransactions :: !(Vector (GenLocated ann (Transaction ann)))
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Ord ann) => Validity (Ledger ann) where
  validate l@(Ledger {..}) =
    mconcat
      [ genericValidate l,
        -- TODO all the currencies are consistent
        -- TODO all the account names are declared
        declare "the prices are sorted" $
          partiallyOrderedByTimestamp priceTimestamp ledgerPrices,
        declare "the transactions are sorted" $
          partiallyOrderedByTimestamp transactionTimestamp ledgerTransactions
      ]

instance NFData ann => NFData (Ledger ann)

partiallyOrderedByTimestamp :: (a -> GenLocated ann Timestamp) -> Vector (GenLocated ann a) -> Bool
partiallyOrderedByTimestamp getTimestamp =
  partiallyOrderedBy
    (Timestamp.comparePartially `on` (locatedValue . getTimestamp . locatedValue))

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

data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: !(Vector (GenLocated ann (Posting ann))),
    transactionAttachments :: !(Vector (GenLocated ann (Attachment ann))),
    transactionAssertions :: !(Vector (GenLocated ann (Assertion ann))),
    transactionTags :: !(Map Tag ann)
  }
  deriving stock (Show, Eq, Generic)

instance (Validity ann, Ord ann) => Validity (Transaction ann)

instance NFData ann => NFData (Transaction ann)

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

data Posting ann = Posting
  { postingReal :: !Bool,
    postingAccountName :: !(GenLocated ann AccountName),
    -- Note: This field will have the source location of the currency _symbol_
    -- that defined it.
    postingCurrency :: !(GenLocated ann (Currency ann)),
    -- Note: This field will have the source location of the decimal literal
    -- that defined it.
    postingAccount :: !(GenLocated ann Money.Account),
    postingCost :: !(Maybe (GenLocated ann (Cost ann))),
    -- Note: This field will have the source location of the percentage
    -- expression that defined it.
    postingPercentage :: !(Maybe (GenLocated ann (Percentage ann)))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Posting ann)

instance NFData ann => NFData (Posting ann)

data Cost ann = Cost
  { -- Note: This field will have the source location of the decimal literal in the cost
    costConversionRate :: !(GenLocated ann ConversionRate),
    -- Note: This field will have the source location of the currency _symbol_ in the cost
    costCurrency :: !(GenLocated ann (Currency ann))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Cost ann)

instance NFData ann => NFData (Cost ann)

newtype Percentage ann = Percentage
  { -- Note: This field will have the source location of the decimal literal in
    -- the percentage expression
    -- Note: This field does not contain a percentage anymore. I.e. the /100
    -- has already been applied. It is just called this because of what it's
    -- called in the module. TODO maybe we want to rename it?
    unPercentage :: GenLocated ann (Ratio Natural)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Percentage ann)

instance NFData ann => NFData (Percentage ann)

data Currency ann = Currency
  { currencySymbol :: !CurrencySymbol,
    -- Note: This field will have the source location of currency _declaration_ that defined it
    currencyQuantisationFactor :: !(GenLocated ann QuantisationFactor)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ann => Validity (Currency ann)

instance NFData ann => NFData (Currency ann)
