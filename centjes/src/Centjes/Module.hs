{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module
  ( LModule,
    Module (..),
    LImport,
    Import (..),
    LDeclaration,
    Declaration (..),
    LCurrencyDeclaration,
    CurrencyDeclaration (..),
    CurrencySymbol (..),
    LAccountDeclaration,
    AccountDeclaration (..),
    LAccountExtra,
    AccountExtra (..),
    LAccountAssertion,
    AccountAssertion (..),
    AccountType (..),
    LTagDeclaration,
    TagDeclaration (..),
    LPriceDeclaration,
    PriceDeclaration (..),
    priceDeclarationCurrencySymbols,
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
    LExtraAttachment,
    ExtraAttachment (..),
    LAttachment,
    Attachment (..),
    LExtraAssertion,
    ExtraAssertion (..),
    LAssertion,
    Assertion (..),
    LExtraTag,
    ExtraTag (..),
    LTag,
    Tag (..),
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
import Centjes.Tag
import Centjes.Timestamp
import Control.Arrow (left)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Money.Amount (Rounding (..))
import Numeric.DecimalLiteral
import Path

type LModule = Module SourceSpan

-- | Module
--
-- A file with imports and declarations
data Module ann = Module
  { -- | Import declaration
    --
    -- @
    -- import bank.cent
    -- @
    moduleImports :: [GenLocated ann (Import ann)],
    -- | Other declarations such as currencies,accounts,transactions,...
    moduleDeclarations :: [GenLocated ann (Declaration ann)]
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (Module ann)

type LDeclaration = LLocated Declaration

-- | Declaration
--
-- A single declaration.
data Declaration ann
  = -- | Comment
    --
    -- @
    -- -- This is a comment
    -- @
    DeclarationComment !(GenLocated ann Text)
  | -- | Currency declaration
    --
    -- @
    -- currency EUR 0.01
    -- @
    DeclarationCurrency !(GenLocated ann (CurrencyDeclaration ann))
  | -- | Account declaration
    --
    -- @
    -- account assets
    -- @
    DeclarationAccount !(GenLocated ann (AccountDeclaration ann))
  | -- | Tag declaration
    --
    -- @
    -- tag deductible
    -- @
    DeclarationTag !(GenLocated ann (TagDeclaration ann))
  | -- | Price declaration
    --
    -- @
    -- price 2024-05-16 USD 1.00 EUR
    -- @
    DeclarationPrice !(GenLocated ann (PriceDeclaration ann))
  | -- | Transaction declaration
    --
    -- @
    -- 2024-05-16
    --   | Description
    --   * assets -5 USD @ 1 EUR
    --   * expenses -5 EUR
    --   + attach receipt.pdf
    --   + tag deductible
    --   + assert assets = 0 USD
    -- @
    DeclarationTransaction !(GenLocated ann (Transaction ann))
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (Declaration ann)

type LImport = LLocated Import

-- | Import declaration
--
-- @
-- bank.cent
-- @
newtype Import ann = Import {importFile :: GenLocated ann (Path Rel File)}
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (Import ann)

type LCurrencyDeclaration = LLocated CurrencyDeclaration

-- | Currency declaration
--
-- @
-- USD 0.01
-- @
data CurrencyDeclaration ann = CurrencyDeclaration
  { currencyDeclarationSymbol :: !(GenLocated ann CurrencySymbol),
    currencyDeclarationQuantisationFactor :: !(GenLocated ann DecimalLiteral)
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (CurrencyDeclaration ann)

type LAccountDeclaration = LLocated AccountDeclaration

-- | Account declaration declaration
--
-- @
-- assets
-- @
--
-- or
--
-- @
-- fancyname assets
-- @
data AccountDeclaration ann = AccountDeclaration
  { accountDeclarationName :: !(GenLocated ann AccountName),
    accountDeclarationType :: !(Maybe (GenLocated ann AccountType)),
    accountDeclarationExtras :: ![GenLocated ann (AccountExtra ann)]
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (AccountDeclaration ann)

type LAccountExtra = LLocated AccountExtra

-- | Account extra
data AccountExtra ann
  = -- | Attachment
    --
    -- @
    -- + attach receipt.pdf
    -- @
    AccountExtraAttachment (GenLocated ann (ExtraAttachment ann))
  | -- | Assertion
    --
    -- @
    -- + assert assets = 5 USD
    -- @
    AccountExtraAssertion (GenLocated ann (AccountAssertion ann))
  | -- | Tag
    --
    -- @
    -- + tag deductible
    -- @
    AccountExtraTag (GenLocated ann (ExtraTag ann))
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (AccountExtra ann)

type LAccountAssertion = LLocated AccountAssertion

data AccountAssertion ann
  = AccountAssertionCurrency !(GenLocated ann CurrencySymbol)
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (AccountAssertion ann)

type LTagDeclaration = LLocated TagDeclaration

-- | Tag declaration
--
-- @
-- deductible
-- @
newtype TagDeclaration ann = TagDeclaration
  { tagDeclarationTag :: GenLocated ann Tag
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (TagDeclaration ann)

type LPriceDeclaration = LLocated PriceDeclaration

-- | Price declaration
--
-- @
-- 2024-05-16 USD 1.00 CHF
-- @
data PriceDeclaration ann = PriceDeclaration
  { priceDeclarationTimestamp :: !(GenLocated ann Timestamp),
    priceDeclarationCurrencySymbol :: !(GenLocated ann CurrencySymbol),
    -- | How many olds for one new
    -- This is of unit: old/new
    priceDeclarationCost :: !(GenLocated ann (CostExpression ann))
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (PriceDeclaration ann)

priceDeclarationCurrencySymbols :: PriceDeclaration ann -> Set CurrencySymbol
priceDeclarationCurrencySymbols PriceDeclaration {..} =
  let Located _ ps = priceDeclarationCurrencySymbol
      Located _ cd = priceDeclarationCost
      Located _ cs = costExpressionCurrencySymbol cd
   in S.fromList [ps, cs]

type LTransaction = LLocated Transaction

-- | Transaction
--
-- @
-- 2024-05-16
--   | Description
--   * assets -5 USD @ 1 EUR
--   * expenses -5 EUR
--   + attach receipt.pdf
--   + tag deductible
--   + assert assets = 0 USD
-- @
data Transaction ann = Transaction
  { transactionTimestamp :: !(GenLocated ann Timestamp),
    transactionDescription :: !(Maybe (GenLocated ann Description)),
    transactionPostings :: ![GenLocated ann (Posting ann)],
    transactionExtras :: ![GenLocated ann (TransactionExtra ann)]
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (Transaction ann)

transactionCurrencySymbols :: Transaction ann -> Set CurrencySymbol
transactionCurrencySymbols Transaction {..} =
  S.unions $
    map
      ( \(Located _ Posting {..}) ->
          S.unions
            [ S.singleton (locatedValue postingCurrencySymbol),
              maybe
                S.empty
                ( S.singleton
                    . locatedValue
                    . costExpressionCurrencySymbol
                    . locatedValue
                )
                postingCost
            ]
      )
      transactionPostings

type LPosting = LLocated Posting

-- | Posting
--
-- @
-- assets -5 USD @ 1 EUR
-- @
data Posting ann = Posting
  { postingReal :: !Bool,
    postingAccountName :: !(GenLocated ann AccountName),
    postingAccount :: !(GenLocated ann DecimalLiteral),
    postingCurrencySymbol :: !(GenLocated ann CurrencySymbol),
    postingCost :: !(Maybe (GenLocated ann (CostExpression ann))),
    postingPercentage :: !(Maybe (GenLocated ann (PercentageExpression ann)))
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (Posting ann)

type LCostExpression = LLocated CostExpression

-- | Cost expression
--
-- @
-- 1 EUR
-- @
--
-- or
--
-- @
-- 1 / 1 EUR
-- @
data CostExpression ann = CostExpression
  { costExpressionConversionRate :: !(GenLocated ann (RationalExpression ann)),
    costExpressionCurrencySymbol :: !(GenLocated ann CurrencySymbol)
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (CostExpression ann)

type LPercentageExpression = LLocated PercentageExpression

-- | Percentage expression
--
-- @
-- 50 %
-- @
--
-- or
--
-- @
-- 1 / 2 %
-- @
data PercentageExpression ann = PercentageExpression
  { percentageExpressionInclusive :: !(Maybe Bool),
    percentageExpressionRounding :: !(Maybe Rounding),
    percentageExpressionRationalExpression :: !(GenLocated ann (RationalExpression ann))
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (PercentageExpression ann)

type LRationalExpression = LLocated RationalExpression

-- | Rational expression
--
-- @
-- 50 %
-- @
--
-- or
--
-- @
-- 1 / 2 %
-- @
data RationalExpression ann
  = RationalExpressionDecimal !(GenLocated ann DecimalLiteral)
  | RationalExpressionFraction
      -- | Numerator
      !(GenLocated ann DecimalLiteral)
      -- | Denominator
      !(GenLocated ann DecimalLiteral)
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (RationalExpression ann)

type LTransactionExtra = LLocated TransactionExtra

-- | Transaction extra
data TransactionExtra ann
  = -- | Attachment
    --
    -- @
    -- + attach receipt.pdf
    -- @
    TransactionAttachment (GenLocated ann (ExtraAttachment ann))
  | -- | Assertion
    --
    -- @
    -- + assert assets = 5 USD
    -- @
    TransactionAssertion (GenLocated ann (ExtraAssertion ann))
  | -- | Tag
    --
    -- @
    -- + tag deductible
    -- @
    TransactionTag (GenLocated ann (ExtraTag ann))
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (TransactionExtra ann)

-- | Attachmnet
--
-- @
-- attach receipt.pdf
-- @
type LExtraAttachment = LLocated ExtraAttachment

newtype ExtraAttachment ann = ExtraAttachment {unExtraAttachment :: GenLocated ann (Attachment ann)}
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (ExtraAttachment ann)

type LAttachment = LLocated Attachment

-- | Attachmnet
--
-- @
-- receipt.pdf
-- @
newtype Attachment ann = Attachment {attachmentPath :: GenLocated ann (Path Rel File)}
  deriving stock (Show, Eq, Generic)

instance (Validity ann) => Validity (Attachment ann)

type LExtraAssertion = LLocated ExtraAssertion

-- | Assertion
--
-- @
-- assert assets = 5 USD
-- @
newtype ExtraAssertion ann = ExtraAssertion {unExtraAssertion :: GenLocated ann (Assertion ann)}
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (ExtraAssertion ann)

type LAssertion = LLocated Assertion

-- | Assertion
--
-- @
-- assets = 5 USD
-- @
data Assertion ann
  = AssertionEquals
      !(GenLocated ann AccountName)
      !(GenLocated ann DecimalLiteral)
      !(GenLocated ann CurrencySymbol)
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (Assertion ann)

type LExtraTag = LLocated ExtraTag

-- | Tag
--
-- @
-- tag deductible
-- @
newtype ExtraTag ann = ExtraTag {unExtraTag :: GenLocated ann Tag}
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (ExtraTag ann)

type LTag = Located Tag

instance HasCodec (Path Rel File) where
  codec = bimapCodec (left show . parseRelFile) fromRelFile codec <?> "relative filepath"
