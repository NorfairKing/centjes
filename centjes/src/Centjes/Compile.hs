{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile (compileDeclarations) where

import Centjes.Ledger as Ledger
import Centjes.Module as Module
import Data.List (sort)
import qualified Data.Vector as V

compileDeclarations :: [Declaration] -> Ledger
compileDeclarations declarations =
  let transactions =
        map
          (\(DeclarationTransaction t) -> t)
          declarations
      ledgerTransactions = V.fromList $ sort $ map compileTransaction transactions
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
