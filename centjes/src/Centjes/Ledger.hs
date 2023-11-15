module Centjes.Ledger where

newtype Ledger = Ledger {ledgerTransactions :: [Transaction]}
