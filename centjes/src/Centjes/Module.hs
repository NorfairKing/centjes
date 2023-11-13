{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Module where

import Control.DeepSeq
import Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)

data Module = Module {moduleTransactions :: ![Declaration]}
  deriving stock (Show, Eq, Generic)

instance Validity Module

instance NFData Module

data Declaration = DeclarationTransaction !Transaction
  deriving stock (Show, Eq, Generic)

instance Validity Declaration

instance NFData Declaration

data Transaction = Transaction
  { transactionTimestamp :: !Timestamp,
    transactionPostings :: ![Posting]
  }
  deriving stock (Show, Eq, Generic)

instance Validity Transaction

instance NFData Transaction

data Posting = Posting
  { postingAccountName :: !AccountName,
    postingAmount :: !Money.Account
  }
  deriving stock (Show, Eq, Generic)

instance Validity Posting

instance NFData Posting

type Timestamp = Day

newtype AccountName = AccountName {unAccountName :: Text}
  deriving (Show, Eq, Generic)

instance Validity AccountName where
  validate an@(AccountName t) =
    mconcat
      [ genericValidate an,
        declare "The account name is not empty" $ not (T.null t),
        declare "The account name starts with an alphabetic character" $ case T.uncons t of
          Nothing -> False
          Just (h, _) -> Char.isAlpha h,
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character or _ or :" $
          case c of
            ':' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

instance NFData AccountName
