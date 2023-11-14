{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes
  ( runCentjes,
  )
where

import Centjes.Module
import Data.Maybe
import Data.Time
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)

runCentjes :: IO ()
runCentjes = do
  let exampleTransaction =
        Transaction
          { transactionTimestamp = fromGregorian 2013 11 13,
            transactionDescription = Description "Example",
            transactionPostings =
              [ Posting
                  { postingAccountName = AccountName "expenses:food",
                    postingAmount = fromJust $ Account.fromMinimalQuantisations 100
                  },
                Posting
                  { postingAccountName = AccountName "assets:cash",
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
