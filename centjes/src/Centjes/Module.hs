{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module
  ( LModule,
    Module (..),
    Import (..),
    LDeclaration,
    Declaration (..),
    LCurrencyDeclaration,
    CurrencyDeclaration (..),
    CurrencySymbol (..),
    LAccountDeclaration,
    AccountDeclaration (..),
    AccountType (..),
    LPriceDeclaration,
    PriceDeclaration (..),
    LCostExpression,
    CostExpression (..),
    LPercentageExpression,
    PercentageExpression (..),
    LRationalExpression,
    RationalExpression (..),
    LTransaction,
    Transaction (..),
    transactionCurrencySymbols,
    Timestamp (..),
    Description (..),
    LPosting,
    Posting (..),
    LTransactionExtra,
    TransactionExtra (..),
    LAttachment,
    Attachment (..),
    LAssertion,
    Assertion (..),
    LTag,
    Tag (..),
    validateTagText,
    AccountName (..),
    DecimalLiteral (..),
  )
where

import Autodocodec
import Centjes.AccountName
import Centjes.AccountType
import Centjes.CurrencySymbol
import Centjes.Description
import Centjes.Location
import Centjes.Timestamp
import Control.Arrow (left)
import Control.DeepSeq
import Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Numeric.DecimalLiteral
import Path

type LModule = Module SourceSpan

data Module ann = Module
  { moduleImports :: [GenLocated ann Import],
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
  | DeclarationPrice !(GenLocated ann (PriceDeclaration ann))
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

data AccountDeclaration ann = AccountDeclaration
  { accountDeclarationName :: !(GenLocated ann AccountName),
    accountDeclarationType :: !(Maybe (GenLocated ann AccountType))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (AccountDeclaration ann)

instance NFData ann => NFData (AccountDeclaration ann)

type LPriceDeclaration = LLocated PriceDeclaration

data PriceDeclaration ann = PriceDeclaration
  { priceDeclarationTimestamp :: !(GenLocated ann Timestamp),
    priceDeclarationCurrencySymbol :: !(GenLocated ann CurrencySymbol),
    -- | How many olds for one new
    -- This is of unit: old/new
    priceDeclarationCost :: !(GenLocated ann (CostExpression ann))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (PriceDeclaration ann)

instance NFData ann => NFData (PriceDeclaration ann)

type LTransaction = LLocated Transaction

data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: ![GenLocated ann (Posting ann)],
    transactionExtras :: ![GenLocated ann (TransactionExtra ann)]
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Transaction ann)

instance NFData ann => NFData (Transaction ann)

transactionCurrencySymbols :: Transaction ann -> Set CurrencySymbol
transactionCurrencySymbols = S.fromList . map (locatedValue . postingCurrencySymbol . locatedValue) . transactionPostings

type LPosting = LLocated Posting

data Posting ann = Posting
  { postingReal :: !Bool,
    postingAccountName :: !(GenLocated ann AccountName),
    postingAccount :: !(GenLocated ann DecimalLiteral),
    postingCurrencySymbol :: !(GenLocated ann CurrencySymbol),
    postingCost :: !(Maybe (GenLocated ann (CostExpression ann))),
    postingPercentage :: !(Maybe (GenLocated ann (PercentageExpression ann)))
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Posting ann)

instance NFData ann => NFData (Posting ann)

type LCostExpression = LLocated CostExpression

data CostExpression ann = CostExpression
  { costExpressionConversionRate :: !(GenLocated ann (RationalExpression ann)),
    costExpressionCurrencySymbol :: !(GenLocated ann CurrencySymbol)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (CostExpression ann)

instance NFData ann => NFData (CostExpression ann)

type LPercentageExpression = LLocated PercentageExpression

newtype PercentageExpression ann = PercentageExpression
  { unPercentageExpression :: GenLocated ann (RationalExpression ann)
  }
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (PercentageExpression ann)

instance NFData ann => NFData (PercentageExpression ann)

type LRationalExpression = LLocated RationalExpression

data RationalExpression ann
  = RationalExpressionDecimal !(GenLocated ann DecimalLiteral)
  | RationalExpressionFraction
      !(GenLocated ann DecimalLiteral)
      -- ^ Numerator
      !(GenLocated ann DecimalLiteral)
      -- ^ Denominator
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (RationalExpression ann)

instance NFData ann => NFData (RationalExpression ann)

type LTransactionExtra = LLocated TransactionExtra

data TransactionExtra ann
  = TransactionAttachment (GenLocated ann (Attachment ann))
  | TransactionAssertion (GenLocated ann (Assertion ann))
  | TransactionTag (GenLocated ann (Tag ann))
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (TransactionExtra ann)

instance NFData ann => NFData (TransactionExtra ann)

type LAttachment = LLocated Attachment

newtype Attachment ann = Attachment {attachmentPath :: GenLocated ann (Path Rel File)}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ann => Validity (Attachment ann)

instance NFData ann => NFData (Attachment ann)

type LAssertion = LLocated Assertion

data Assertion ann
  = AssertionEquals
      !(GenLocated ann AccountName)
      !(GenLocated ann DecimalLiteral)
      !(GenLocated ann CurrencySymbol)
  deriving stock (Show, Eq, Generic)

instance Validity ann => Validity (Assertion ann)

instance NFData ann => NFData (Assertion ann)

type LTag = LLocated Tag

newtype Tag ann = Tag {tagText :: GenLocated ann Text}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ann => Validity (Tag ann) where
  validate tag@(Tag (Located _ t)) =
    mconcat
      [ genericValidate tag,
        declare "the text is nonempty" $ not $ T.null t,
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character, or _, or -, or :" $
          case c of
            ':' -> True
            '-' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

validateTagText :: Text -> Maybe Text
validateTagText t = locatedValue . tagText <$> constructValid (Tag (Located () t))

instance NFData ann => NFData (Tag ann)

instance HasCodec (Path Rel File) where
  codec = bimapCodec (left show . parseRelFile) fromRelFile codec <?> "relative filepath"
