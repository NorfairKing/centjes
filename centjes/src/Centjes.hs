{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Centjes
  ( runCentjes,
  )
where

import Brick
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State
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
runCentjes = pure ()

data Transaction = Transaction {transactionPostings :: [Posting]}
  deriving stock (Show, Eq, Generic)

data Posting = Posting
  { postingAccountName :: AccountName,
    postingAmount :: Money.Account
  }
  deriving stock (Show, Eq, Generic)

type AccountName = String
