{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Module where

import Autodocodec
import Centjes.DecimalLiteral
import Centjes.Location
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
import GHC.Generics (Generic)
import Path

type LModule = Module SourceSpan

data Module ann = Module
  { moduleImports :: [Import],
    moduleDeclarations :: [Declaration ann]
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Module ann)

instance NFData ann => NFData (Module ann)

type LDeclaration = Declaration SourceSpan

data Declaration ann
  = DeclarationComment !(GenLocated ann Text)
  | DeclarationCurrency !(GenLocated ann (CurrencyDeclaration ann))
  | DeclarationAccount !(GenLocated ann (AccountDeclaration ann))
  | DeclarationTransaction !(GenLocated ann (Transaction ann))
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Declaration ann)

instance NFData ann => NFData (Declaration ann)

newtype Import = Import {importFile :: Path Rel File}
  deriving stock (Show, Eq, Generic)

instance Validity Import

instance NFData Import

type LCurrencyDeclaration = LLocated CurrencyDeclaration

data CurrencyDeclaration ann = CurrencyDeclaration
  { currencyDeclarationSymbol :: !(GenLocated ann CurrencySymbol),
    currencyDeclarationQuantisationFactor :: !(GenLocated ann DecimalLiteral)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (CurrencyDeclaration ann)

instance NFData ann => NFData (CurrencyDeclaration ann)

type LAccountDeclaration = LLocated AccountDeclaration

newtype AccountDeclaration ann = AccountDeclaration
  { accountDeclarationName :: GenLocated ann AccountName
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (AccountDeclaration ann)

instance NFData ann => NFData (AccountDeclaration ann)

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity CurrencySymbol where
  validate an@(CurrencySymbol t) =
    mconcat
      [ genericValidate an,
        declare "The currency symbol is not empty" $ not (T.null t),
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character, or _, or -, or :" $
          case c of
            ':' -> True
            '-' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

instance NFData CurrencySymbol

type LTransaction = LLocated Transaction

data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: ![GenLocated ann (Posting ann)]
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Transaction ann)

instance NFData ann => NFData (Transaction ann)

transactionCurrencySymbols :: Transaction ann -> Set CurrencySymbol
transactionCurrencySymbols = S.fromList . map (locatedValue . postingCurrencySymbol . locatedValue) . transactionPostings

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

type LPosting = LLocated Posting

data Posting ann = Posting
  { postingAccountName :: !(GenLocated ann AccountName),
    postingAccount :: !(GenLocated ann DecimalLiteral),
    postingCurrencySymbol :: !(GenLocated ann CurrencySymbol)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Posting ann)

instance NFData ann => NFData (Posting ann)

newtype Timestamp = Timestamp {timestampDay :: Day}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Timestamp

instance NFData Timestamp

-- TODO Rename unAccountName to accountNameText
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

instance HasCodec AccountName where
  -- TODO actual parsing
  codec = dimapCodec AccountName unAccountName codec
