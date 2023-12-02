{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Ledger.Gen where

import Centjes.DecimalLiteral.Gen ()
import Centjes.Ledger
import Centjes.Location
import Centjes.Module.Gen ()
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Vector ()
import Data.List (sortOn)
import qualified Data.Vector as V

instance (GenValid ann, Eq ann) => GenValid (Ledger ann) where
  genValid = do
    ledgerCurrencies <- genValid
    ledgerTransactions <- V.fromList . sortOn (locatedValue . transactionTimestamp . locatedValue) <$> genValid
    pure Ledger {..}
  shrinkValid l =
    filter (/= l)
      . map (\l' -> l' {ledgerTransactions = V.fromList (sortOn (locatedValue . transactionTimestamp . locatedValue) (V.toList (ledgerTransactions l')))})
      $ shrinkValidStructurally l

instance GenValid ann => GenValid (Transaction ann)

instance GenValid ann => GenValid (Posting ann)

instance GenValid ann => GenValid (Currency ann)
