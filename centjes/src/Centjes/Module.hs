{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Centjes.Module where

import Data.Time
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)

data Module = Module {moduleTransactions :: ![Declaration]}
  deriving stock (Show, Eq, Generic)

data Declaration = DeclarationTransaction !Transaction
  deriving stock (Show, Eq, Generic)

data Transaction = Transaction
  { transactionTimestamp :: !Timestamp,
    transactionPostings :: ![Posting]
  }
  deriving stock (Show, Eq, Generic)

data Posting = Posting
  { postingAccountName :: !AccountName,
    postingAmount :: !Money.Account
  }
  deriving stock (Show, Eq, Generic)

type Timestamp = Day

type AccountName = String
