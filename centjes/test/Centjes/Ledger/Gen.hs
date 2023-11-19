{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Ledger.Gen where

import Centjes.DecimalLiteral.Gen ()
import Centjes.Ledger
import Centjes.Module.Gen ()
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Text
import Data.GenValidity.Vector ()
import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V
import Money.Account.Gen ()
import Money.QuantisationFactor.Gen ()
import Path
import Test.QuickCheck

instance GenValid Ledger where
  genValid = do
    ledgerCurrencies <- genValid
    ledgerTransactions <- V.fromList . sort <$> genValid
    pure Ledger {..}
  shrinkValid l =
    filter (/= l)
      -- TODO sort the vector directly?
      . map (\l -> l {ledgerTransactions = V.fromList (sort (V.toList (ledgerTransactions l)))})
      $ shrinkValidStructurally l

instance GenValid Transaction

instance GenValid Posting

instance GenValid Currency
