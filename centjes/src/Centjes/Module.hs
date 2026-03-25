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
    AccountAssertionVirtual (..),
    AccountType (..),
    LTagDeclaration,
    TagDeclaration (..),
    LPriceDeclaration,
    PriceDeclaration (..),
    priceDeclarationCurrencySymbols,
    stripDeclarationAnnotation,
    stripPriceDeclarationAnnotation,
    LCostExpression,
    CostExpression (..),
    LRatioExpression,
    RatioExpression (..),
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
  | AccountAssertionVirtual !(GenLocated ann AccountAssertionVirtual)
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (AccountAssertion ann)

data AccountAssertionVirtual
  = AccountAssertionNoVirtual
  | AccountAssertionVirtualAllowed
  | AccountAssertionVirtualOnly
  deriving stock (Show, Generic)

instance Validity AccountAssertionVirtual

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

stripPriceDeclarationAnnotation :: PriceDeclaration ann -> PriceDeclaration ()
stripPriceDeclarationAnnotation PriceDeclaration {..} =
  let Located _ timestamp = priceDeclarationTimestamp
      Located _ currencySymbol = priceDeclarationCurrencySymbol
      Located _ costExpression = priceDeclarationCost
   in PriceDeclaration
        { priceDeclarationTimestamp = noLoc timestamp,
          priceDeclarationCurrencySymbol = noLoc currencySymbol,
          priceDeclarationCost = noLoc (stripCostExpressionAnnotation costExpression)
        }

stripCostExpressionAnnotation :: CostExpression ann -> CostExpression ()
stripCostExpressionAnnotation CostExpression {..} =
  let Located _ conversionRate = costExpressionConversionRate
      Located _ currencySymbol = costExpressionCurrencySymbol
   in CostExpression
        { costExpressionConversionRate = noLoc (stripRationalExpressionAnnotation conversionRate),
          costExpressionCurrencySymbol = noLoc currencySymbol
        }

stripRationalExpressionAnnotation :: RationalExpression ann -> RationalExpression ()
stripRationalExpressionAnnotation RationalExpression {..} =
  RationalExpression
    { rationalExpressionNumerator = noLoc (locatedValue rationalExpressionNumerator),
      rationalExpressionDenominator = fmap (noLoc . locatedValue) rationalExpressionDenominator,
      rationalExpressionPercent = rationalExpressionPercent
    }

stripDeclarationAnnotation :: Declaration ann -> Declaration ()
stripDeclarationAnnotation (DeclarationComment (Located _ t)) = DeclarationComment (noLoc t)
stripDeclarationAnnotation (DeclarationCurrency (Located _ cd)) = DeclarationCurrency (noLoc (stripCurrencyDeclarationAnnotation cd))
stripDeclarationAnnotation (DeclarationAccount (Located _ ad)) = DeclarationAccount (noLoc (stripAccountDeclarationAnnotation ad))
stripDeclarationAnnotation (DeclarationTag (Located _ td)) = DeclarationTag (noLoc (stripTagDeclarationAnnotation td))
stripDeclarationAnnotation (DeclarationPrice (Located _ pd)) = DeclarationPrice (noLoc (stripPriceDeclarationAnnotation pd))
stripDeclarationAnnotation (DeclarationTransaction (Located _ t)) = DeclarationTransaction (noLoc (stripTransactionAnnotation t))

stripCurrencyDeclarationAnnotation :: CurrencyDeclaration ann -> CurrencyDeclaration ()
stripCurrencyDeclarationAnnotation CurrencyDeclaration {..} =
  CurrencyDeclaration
    { currencyDeclarationSymbol = noLoc (locatedValue currencyDeclarationSymbol),
      currencyDeclarationQuantisationFactor = noLoc (locatedValue currencyDeclarationQuantisationFactor)
    }

stripAccountDeclarationAnnotation :: AccountDeclaration ann -> AccountDeclaration ()
stripAccountDeclarationAnnotation AccountDeclaration {..} =
  AccountDeclaration
    { accountDeclarationName = noLoc (locatedValue accountDeclarationName),
      accountDeclarationType = fmap (noLoc . locatedValue) accountDeclarationType,
      accountDeclarationExtras = map (noLoc . stripAccountExtraAnnotation . locatedValue) accountDeclarationExtras
    }

stripAccountExtraAnnotation :: AccountExtra ann -> AccountExtra ()
stripAccountExtraAnnotation (AccountExtraAttachment (Located _ ea)) = AccountExtraAttachment (noLoc (stripExtraAttachmentAnnotation ea))
stripAccountExtraAnnotation (AccountExtraAssertion (Located _ aa)) = AccountExtraAssertion (noLoc (stripAccountAssertionAnnotation aa))
stripAccountExtraAnnotation (AccountExtraTag (Located _ et)) = AccountExtraTag (noLoc (stripExtraTagAnnotation et))

stripAccountAssertionAnnotation :: AccountAssertion ann -> AccountAssertion ()
stripAccountAssertionAnnotation (AccountAssertionCurrency (Located _ cs)) = AccountAssertionCurrency (noLoc cs)
stripAccountAssertionAnnotation (AccountAssertionVirtual (Located _ v)) = AccountAssertionVirtual (noLoc v)

stripTagDeclarationAnnotation :: TagDeclaration ann -> TagDeclaration ()
stripTagDeclarationAnnotation TagDeclaration {..} =
  TagDeclaration {tagDeclarationTag = noLoc (locatedValue tagDeclarationTag)}

stripTransactionAnnotation :: Transaction ann -> Transaction ()
stripTransactionAnnotation Transaction {..} =
  Transaction
    { transactionTimestamp = noLoc (locatedValue transactionTimestamp),
      transactionDescription = fmap (noLoc . locatedValue) transactionDescription,
      transactionPostings = map (noLoc . stripPostingAnnotation . locatedValue) transactionPostings,
      transactionExtras = map (noLoc . stripTransactionExtraAnnotation . locatedValue) transactionExtras
    }

stripPostingAnnotation :: Posting ann -> Posting ()
stripPostingAnnotation Posting {..} =
  Posting
    { postingReal = postingReal,
      postingAccountName = noLoc (locatedValue postingAccountName),
      postingAccount = noLoc (locatedValue postingAccount),
      postingCurrencySymbol = noLoc (locatedValue postingCurrencySymbol),
      postingCost = fmap (noLoc . stripCostExpressionAnnotation . locatedValue) postingCost,
      postingRatio = fmap (noLoc . stripRatioExpressionAnnotation . locatedValue) postingRatio
    }

stripRatioExpressionAnnotation :: RatioExpression ann -> RatioExpression ()
stripRatioExpressionAnnotation RatioExpression {..} =
  RatioExpression
    { ratioExpressionInclusive = ratioExpressionInclusive,
      ratioExpressionRounding = ratioExpressionRounding,
      ratioExpressionRationalExpression = noLoc (stripRationalExpressionAnnotation (locatedValue ratioExpressionRationalExpression))
    }

stripTransactionExtraAnnotation :: TransactionExtra ann -> TransactionExtra ()
stripTransactionExtraAnnotation (TransactionAttachment (Located _ ea)) = TransactionAttachment (noLoc (stripExtraAttachmentAnnotation ea))
stripTransactionExtraAnnotation (TransactionAssertion (Located _ ea)) = TransactionAssertion (noLoc (stripExtraAssertionAnnotation ea))
stripTransactionExtraAnnotation (TransactionTag (Located _ et)) = TransactionTag (noLoc (stripExtraTagAnnotation et))

stripExtraAttachmentAnnotation :: ExtraAttachment ann -> ExtraAttachment ()
stripExtraAttachmentAnnotation (ExtraAttachment (Located _ a)) = ExtraAttachment (noLoc (stripAttachmentAnnotation a))

stripAttachmentAnnotation :: Attachment ann -> Attachment ()
stripAttachmentAnnotation (Attachment (Located _ p)) = Attachment (noLoc p)

stripExtraAssertionAnnotation :: ExtraAssertion ann -> ExtraAssertion ()
stripExtraAssertionAnnotation (ExtraAssertion (Located _ a)) = ExtraAssertion (noLoc (stripAssertionAnnotation a))

stripAssertionAnnotation :: Assertion ann -> Assertion ()
stripAssertionAnnotation (AssertionEquals (Located _ an) (Located _ dl) (Located _ cs)) =
  AssertionEquals (noLoc an) (noLoc dl) (noLoc cs)

stripExtraTagAnnotation :: ExtraTag ann -> ExtraTag ()
stripExtraTagAnnotation (ExtraTag (Located _ t)) = ExtraTag (noLoc t)

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
    postingRatio :: !(Maybe (GenLocated ann (RatioExpression ann)))
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

type LRatioExpression = LLocated RatioExpression

-- | Ratio expression
--
-- @
-- ~:e 50 %
-- @
data RatioExpression ann = RatioExpression
  { ratioExpressionInclusive :: !(Maybe Bool),
    ratioExpressionRounding :: !(Maybe Rounding),
    ratioExpressionRationalExpression :: !(GenLocated ann (RationalExpression ann))
  }
  deriving stock (Show, Generic)

instance (Validity ann) => Validity (RatioExpression ann)

type LRationalExpression = LLocated RationalExpression

-- | Rational expression
--
-- @
-- 50
-- @
--
-- or
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
--
-- or
--
-- @
-- 1 / 2
-- @
data RationalExpression ann = RationalExpression
  { rationalExpressionNumerator :: !(GenLocated ann DecimalLiteral),
    rationalExpressionDenominator :: !(Maybe (GenLocated ann DecimalLiteral)),
    rationalExpressionPercent :: !Bool
  }
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
