{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Compile
  ( CompileError (..),
    compileDeclarations,
    compileTransaction,
    compilePosting,
  )
where

import Centjes.Ledger as Ledger
import Centjes.Module as Module
import Centjes.Validation
import Control.Exception
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Validity (Validity)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)

data CompileError = CompileErrorMissingCurrency CurrencySymbol
  deriving (Show, Eq, Generic)

instance Validity CompileError

instance Exception CompileError where
  displayException = \case
    CompileErrorMissingCurrency symbol -> unwords ["Undeclared currency:", show symbol]

compileDeclarations :: [Declaration] -> Validation CompileError Ledger
compileDeclarations declarations = do
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
  ledgerTransactions <- V.fromList . sort <$> traverse (compileTransaction ledgerCurrencies) transactions
  pure Ledger {..}

compileTransaction :: Map CurrencySymbol Word32 -> Module.Transaction -> Validation CompileError Ledger.Transaction
compileTransaction currencies mt = do
  let transactionTimestamp = Module.transactionTimestamp mt
      transactionDescription = Module.transactionDescription mt
  transactionPostings <- traverse (compilePosting currencies) (Module.transactionPostings mt)
  pure Ledger.Transaction {..}

compilePosting :: Map CurrencySymbol Word32 -> Module.Posting -> Validation CompileError Ledger.Posting
compilePosting currencies mp = do
  let postingAccountName = Module.postingAccountName mp
      postingAccount = Module.postingAccount mp
      symbol = Module.postingCurrencySymbol mp
  postingCurrency <- case M.lookup symbol currencies of
    Nothing -> validationFailure $ CompileErrorMissingCurrency symbol
    Just factor -> pure $ Currency {currencySymbol = symbol, currencyFactor = factor}
  pure Ledger.Posting {..}
