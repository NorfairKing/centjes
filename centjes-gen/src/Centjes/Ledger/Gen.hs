{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Ledger.Gen where

import Centjes.Ledger as Ledger
import Centjes.Location
import Centjes.Module.Gen ()
import Centjes.Timestamp as Timestamp
import Data.Function
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Vector ()
import Data.List (sortBy)
import Data.Maybe
import qualified Data.Vector as V
import Numeric.DecimalLiteral.Gen ()

instance (GenValid ann, Eq ann) => GenValid (Ledger ann) where
  genValid = do
    ledgerPrices <-
      V.fromList
        . sortBy
          ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
              `on` locatedValue
                . Ledger.priceTimestamp
                . locatedValue
          )
        <$> genValid
    ledgerTransactions <-
      V.fromList
        . sortBy
          ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
              `on` locatedValue
                . Ledger.transactionTimestamp
                . locatedValue
          )
        <$> genValid
    pure Ledger {..}
  shrinkValid l =
    filter (/= l)
      . map
        ( \l' ->
            l'
              { ledgerTransactions =
                  V.fromList $
                    sortBy
                      ( (\t1 t2 -> fromMaybe EQ (Timestamp.comparePartially t1 t2))
                          `on` locatedValue
                            . Ledger.transactionTimestamp
                            . locatedValue
                      )
                      (V.toList (ledgerTransactions l'))
              }
        )
      $ shrinkValidStructurally l

instance GenValid ann => GenValid (Price ann)

instance GenValid ann => GenValid (Transaction ann)

instance GenValid ann => GenValid (Assertion ann)

instance GenValid ann => GenValid (Posting ann)

instance GenValid ann => GenValid (Currency ann)
