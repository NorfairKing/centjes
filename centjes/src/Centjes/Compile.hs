{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile where

import Centjes.Ledger as Ledger
import Centjes.Module as Module
import Data.List (sort)

compileModule :: Module -> Ledger
compileModule m =
  let transactions = map (\(DeclarationTransaction t) -> t) (moduleDeclarations m)
      ledgerTransactions = sort $ map compileTransaction transactions
   in Ledger {..}

compileTransaction :: Module.Transaction -> Ledger.Transaction
compileTransaction mt =
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
      transactionPostings = map compilePosting (Module.transactionPostings mt)
   in Ledger.Transaction {..}

compilePosting :: Module.Posting -> Ledger.Posting
compilePosting mp =
  let postingAccountName = Module.postingAccountName mp
      postingAccount = Module.postingAccount mp
   in Ledger.Posting {..}
