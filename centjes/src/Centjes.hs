{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes
  ( runCentjes,
  )
where

import Centjes.Module
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State
import Data.Maybe
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Data.Word
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import System.Random
import Text.Printf

runCentjes :: IO ()
runCentjes = do
  let exampleTransaction =
        Transaction
          { transactionTimestamp = fromGregorian 2013 11 13,
            transactionPostings =
              [ Posting
                  { postingAccountName = "expenses:food",
                    postingAmount = fromJust $ Account.fromMinimalQuantisations 100
                  },
                Posting
                  { postingAccountName = "assets:cash",
                    postingAmount = fromJust $ Account.fromMinimalQuantisations (-100)
                  }
              ]
          }
  print $ balanceTransaction exampleTransaction

data Balancing
  = Balanced
  | OffBalanceBy !Money.Account
  | CouldNotBalance
  deriving stock (Show, Eq, Generic)

balanceTransaction :: Transaction -> Balancing
balanceTransaction Transaction {..} =
  let actualSum = Account.sum (map postingAmount transactionPostings)
   in case actualSum of
        Nothing -> CouldNotBalance
        Just s ->
          if s == Account.zero
            then Balanced
            else OffBalanceBy s
