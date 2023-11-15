{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Ledger
  ( Ledger (..),
    Transaction (..),
    Description (..),
    Posting (..),
    AccountName (..),
    Timestamp,
  )
where

import Centjes.Module (AccountName (..), Description (..), Timestamp)
import Control.DeepSeq
import Data.List (sort)
import Data.Validity
import Data.Validity.Vector ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)

newtype Ledger = Ledger {ledgerTransactions :: Vector Transaction}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Ledger where
  validate l@(Ledger {..}) =
    mconcat
      [ genericValidate l,
        declare "the transactions are sorted" $ ordered ledgerTransactions
      ]

instance NFData Ledger

-- TODO this can probably be much faster
ordered :: Ord a => Vector a -> Bool
ordered v =
  let l = V.toList v
   in sort l == l

-- Note: We sort the transactions with the Ord instance to make sure that all
-- fields are taken into account.
-- In order for that to also sort the transactions by timestamp, the
-- timestamp must be the first field.
data Transaction = Transaction
  { transactionTimestamp :: !Timestamp,
    transactionDescription :: !Description,
    transactionPostings :: ![Posting]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Transaction

instance NFData Transaction

data Posting = Posting
  { postingAccountName :: !AccountName,
    postingAccount :: !Money.Account
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Posting

instance NFData Posting
