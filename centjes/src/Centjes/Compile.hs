{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile (compileDeclarations) where

import Centjes.Ledger as Ledger
import Centjes.Module as Module
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V
import Data.Word

compileDeclarations :: [Declaration] -> Ledger
compileDeclarations declarations =
  let ledgerCurrencies =
        M.fromList $
          map (\CurrencyDeclaration {..} -> (currencyDeclarationSymbol, currencyDeclarationFactor)) $
            mapMaybe
              ( \case
                  DeclarationCurrency cd -> Just cd
                  _ -> Nothing
              )
              declarations
      -- TODO check for duplicate currencies
      transactions =
        mapMaybe
          ( \case
              DeclarationTransaction t -> Just t
              _ -> Nothing
          )
          declarations
      ledgerTransactions = V.fromList $ sort $ map (compileTransaction ledgerCurrencies) transactions
   in Ledger {..}

compileTransaction :: Map CurrencySymbol Word32 -> Module.Transaction -> Ledger.Transaction
compileTransaction currencies mt =
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
      transactionPostings = map (compilePosting currencies) (Module.transactionPostings mt)
   in Ledger.Transaction {..}

compilePosting :: Map CurrencySymbol Word32 -> Module.Posting -> Ledger.Posting
compilePosting currencies mp =
  let postingAccountName = Module.postingAccountName mp
      postingAccount = Module.postingAccount mp
      symbol = Module.postingCurrencySymbol mp
      postingCurrency = case M.lookup symbol currencies of
        Nothing -> error "TODO refactor to allow for failure"
        Just factor -> Currency {currencySymbol = symbol, currencyFactor = factor}
   in Ledger.Posting {..}
