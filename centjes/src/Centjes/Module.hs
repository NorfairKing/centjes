{-# LANGUAGE DeriveGeneric #-}
module Centjes.Module where

data Module =  { moduleTransactions :: [Transaction ] }
  deriving stock (Show, Eq, Generic)

data Transaction = Transaction {transactionPostings :: [Posting]}
  deriving stock (Show, Eq, Generic)

data Posting = Posting
  { postingAccountName :: AccountName,
    postingAmount :: Money.Account
  }
  deriving stock (Show, Eq, Generic)

type AccountName = String

