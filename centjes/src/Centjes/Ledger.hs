{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Ledger
  ( Ledger (..),
    Timestamp (..),
    CurrencySymbol (..),
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
import Centjes.Module (CurrencySymbol (..), Description (..), Timestamp (..))
import Control.DeepSeq
import Data.Validity
import Data.Validity.Map ()
import Data.Validity.Vector ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor

newtype Ledger ann = Ledger
  { ledgerTransactions :: Vector (GenLocated ann (Transaction ann))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Ledger ann) where
  validate l@(Ledger {..}) =
    mconcat
      [ genericValidate l,
        declare "the transactions are sorted" $ ordered $ V.map (locatedValue . transactionTimestamp . locatedValue) ledgerTransactions
      ]

instance NFData ann => NFData (Ledger ann)

ordered :: Ord a => Vector a -> Bool
ordered v =
  if V.null v
    then True
    else V.and (V.zipWith (<=) v (V.tail v))

data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: !(Vector (GenLocated ann (Posting ann))),
    transactionAssertions :: !(Vector (GenLocated ann (Assertion ann)))
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ann => Validity (Transaction ann)

instance NFData ann => NFData (Transaction ann)

data Posting ann = Posting
  { postingAccountName :: !(GenLocated ann AccountName),
    -- Note: This field will have the source location of the currency _symbol_ that defined it.
    postingCurrency :: !(GenLocated ann (Currency ann)),
    -- Note: This field will have the source location of the decimal literal that defined it.
    postingAccount :: !(GenLocated ann Money.Account)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ann => Validity (Posting ann)

instance NFData ann => NFData (Posting ann)

data Assertion ann
  = AssertionEquals
      !(GenLocated ann AccountName)
      -- Note: This field will have the source location of the decimal literal that defined it.
      !(GenLocated ann Money.Account)
      -- Note: This field will have the source location of the currency _symbol_ that defined it
      !(GenLocated ann (Currency ann))
  deriving stock (Show, Eq, Ord, Generic)

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
