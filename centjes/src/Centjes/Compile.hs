{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile where

import Centjes.Ledger as Ledger
import Centjes.Module as Module
import Data.List (sort)
import Data.Maybe
import qualified Data.Vector as V

compileModule :: Module -> Ledger
compileModule m =
  let transactions =
        mapMaybe
          ( \case
              DeclarationTransaction t -> Just t
              _ -> Nothing
          )
          (moduleDeclarations m)
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
