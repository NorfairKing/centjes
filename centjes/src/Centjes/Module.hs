{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Module where

import Control.DeepSeq
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import Data.Word
import GHC.Generics (Generic)
import qualified Money.Account as Money (Account)
import Path

-- TODO roundtrip comments.
data Module = Module
  { moduleImports :: [Import],
    moduleDeclarations :: [Declaration]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Module

instance NFData Module

data Declaration
  = DeclarationCurrency CurrencyDeclaration
  | DeclarationTransaction Transaction
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Declaration

instance NFData Declaration

newtype Import = Import {importFile :: Path Rel File}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Import

instance NFData Import

data CurrencyDeclaration = CurrencyDeclaration
  { currencyDeclarationSymbol :: CurrencySymbol,
    currencyDeclarationFactor :: Word32
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity CurrencyDeclaration

instance NFData CurrencyDeclaration

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity CurrencySymbol where
  validate an@(CurrencySymbol t) =
    mconcat
      [ genericValidate an,
        declare "The currency symbol is not empty" $ not (T.null t),
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character or _ or :" $
          case c of
            ':' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

instance NFData CurrencySymbol

data Transaction = Transaction
  { transactionTimestamp :: !Timestamp,
    transactionDescription :: !Description,
    transactionPostings :: ![Posting]
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Transaction

instance NFData Transaction

transactionCurrencySymbols :: Transaction -> Set CurrencySymbol
transactionCurrencySymbols = S.fromList . map postingCurrencySymbol . transactionPostings

newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Description where
  validate an@(Description t) =
    mconcat
      [ genericValidate an,
        decorateText t validateDescriptionChar
      ]

validateDescriptionChar :: Char -> Validation
validateDescriptionChar = \c -> declare "The character is not a newline, and not a control character" $
  case c of
    '\n' -> False
    '\r' -> False
    _
      | Char.isControl c -> False
      | otherwise -> True

instance NFData Description

nullDescription :: Description -> Bool
nullDescription = T.null . unDescription

data Posting = Posting
  { postingAccountName :: !AccountName,
    postingAccount :: !Money.Account,
    postingCurrencySymbol :: !CurrencySymbol
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Posting

instance NFData Posting

type Timestamp = Day

newtype AccountName = AccountName {unAccountName :: Text}
  deriving (Show, Eq, Ord, Generic)

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
