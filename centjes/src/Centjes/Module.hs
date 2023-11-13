{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Centjes.Module where

import Data.Text (Text)
import Data.Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)

data Module = Module {moduleTransactions :: ![Declaration]}
  deriving stock (Show, Eq, Generic)

instance Validity Module

data Declaration = DeclarationTransaction !Transaction
  deriving stock (Show, Eq, Generic)

instance Validity Declaration

data Transaction = Transaction
  { transactionTimestamp :: !Timestamp,
    transactionPostings :: ![Posting]
  }
  deriving stock (Show, Eq, Generic)

instance Validity Transaction

data Posting = Posting
  { postingAccountName :: !AccountName,
    postingAmount :: !Money.Account
  }
  deriving stock (Show, Eq, Generic)

instance Validity Posting

type Timestamp = Day

type AccountName = Text
