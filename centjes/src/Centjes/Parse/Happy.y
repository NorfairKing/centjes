{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -w #-}
module Centjes.Parse.Happy
  ( parseModule
  , parseDeclaration
  , parseCurrencyDeclaration
  , parseAccountDeclaration
  , parseTagDeclaration
  , parsePriceDeclaration
  , parseTransaction
  ) where

import Centjes.AccountName as AccountName
import Centjes.AccountType as AccountType
import Centjes.Comment (Comment(..))
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Description as Description
import Centjes.Location
import Centjes.Module
import Centjes.Parse.Alex
import Centjes.Tag as Tag
import Centjes.Timestamp as Timestamp
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust)
import Data.Semigroup
import Data.Text (Text)
import Money.Amount (Rounding(..))
import Numeric.DecimalLiteral (DecimalLiteral)
import Path
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

}

-- GHC's Happy file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y

%name moduleParser module
%name declarationParser declaration
%name currencyDeclarationParser currency_dec
%name accountDeclarationParser account_dec
%name tagDeclarationParser tag_dec
%name priceDeclarationParser price_dec
%name transactionParser transaction_dec

%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Located _ TokenEOF }
-- Without this we get a type error
%error { happyError }

-- Don't allow conflicts
%expect 0

%token 
      tok_comment             { Located _ (TokenComment _) }
      tok_indented_comment    { Located _ (TokenIndentedComment _) }
      tok_import          { Located _ TokenImport }
      tok_attach          { Located _ TokenAttach }
      tok_assert          { Located _ TokenAssert }
      tok_assert_virtual  { Located _ TokenAssertVirtual }
      tok_tag             { Located _ TokenTag }
      tok_price           { Located _ TokenPrice }
      tok_lot             { Located _ TokenLot }
      tok_file_path       { Located _ (TokenFilePath _) }
      tok_eq              { Located _ TokenEq }
      tok_timestamp       { Located _ (TokenTimestamp _) }
      tok_var             { Located _ (TokenVar _) }
      tok_pipe            { Located _ TokenPipe }
      tok_anyline         { Located _ (TokenAnyLine _) }
      tok_decimal_literal { Located _ (TokenDecimalLiteral _) }
      tok_plus            { Located _ TokenPlus }
      tok_star            { Located _ TokenStar }
      tok_bang            { Located _ TokenBang }
      tok_at              { Located _ TokenAt }
      tok_slash           { Located _ TokenSlash }
      tok_tilde           { Located _ (TokenTilde _ _) }
      tok_percent         { Located _ TokenPercent }
      tok_currency        { Located _ TokenCurrency}
      tok_account         { Located _ TokenAccount }
      tok_no_virtual      { Located _ TokenNoVirtual }
      tok_virtual_allowed { Located _ TokenVirtualAllowed }
      tok_virtual_only    { Located _ TokenVirtualOnly }


%%

module
  :: { LModule }
  : many(import_dec) declarations { Module $1 $2 }

import_dec
  :: { LImport }
  : tok_import rel_file_exp { sBE $1 $2 $ Import $2 }

-- Note: A run of comment lines is one comment, and this is one chain of
-- productions so that the run appears in one place per parser state.  Letting a
-- list of declarations each start with a run instead is ambiguous: after one
-- comment line, another one could extend the run or start the next declaration,
-- and that is a shift/reduce conflict.
declarations
  :: { [LDeclaration] }
  : comment_run { commentDeclarations $1 }
  | comment_run non_comment_dec declarations { commentDeclarations $1 ++ ($2 : $3) }

comment_run
  :: { Maybe (Located Comment) }
  : many(comment_line) { combineComments $1 }

comment_line
  :: { Located Comment }
  : tok_comment { parseComment $1 }

declaration
  :: { LDeclaration }
  : comment_dec { sL1 $1 $ DeclarationComment $1 }
  | non_comment_dec { $1 }

-- Note: Only reachable as the start symbol of 'parseDeclaration', so a comment
-- line can only be followed by another one here, not by a next declaration.
comment_dec
  :: { Located Comment }
  : some(comment_line) {% requireComments (NE.toList $1) }

non_comment_dec
  :: { LDeclaration }
  : currency_dec { sL1 $1 $ DeclarationCurrency $1 }
  | account_dec { sL1 $1 $ DeclarationAccount $1 }
  | tag_dec { sL1 $1 $ DeclarationTag $1 }
  | price_dec { sL1 $1 $ DeclarationPrice $1 }
  | transaction_dec { sL1 $1 $ DeclarationTransaction $1 }

indented_comments
  :: { Maybe (Located Comment) }
  : many(indented_comment) { combineComments $1 }

indented_comment
  :: { Located Comment }
  : tok_indented_comment { parseComment $1 }

currency_dec
  :: { LCurrencyDeclaration }
  : tok_currency currency_symbol quantisation_factor { sBE $1 $3 $ CurrencyDeclaration $2 $3 }

currency_symbol
  :: { Located CurrencySymbol }
  : tok_var {% parseCurrencySymbol $1 }

quantisation_factor
  :: { Located DecimalLiteral }
  : tok_decimal_literal { parseDecimalLiteral $1 }

account_dec
  :: { LAccountDeclaration }
  : tok_account account_name optional(account_type) account_extras { sBEML $1 $2 $3 (map commentedValue $4) $ AccountDeclaration $2 $3 $4 }

-- Note: The comments above a line belong to it, so each line of a block is
-- preceded by the comments written above it.  Any comments left over at the end
-- of a block belong to no line and are a parse error.
account_extras
  :: { [Commented SourceSpan (AccountExtra SourceSpan)] }
  : indented_comments {% noTrailingComments [] $1 }
  | indented_comments account_extra account_extras
      { Commented $2 $1 : $3 }

account_extra
  :: { LAccountExtra }
  : tok_plus extra_attachment { sBE $1 $2 $ AccountExtraAttachment $2 }
  | tok_plus extra_account_assertion { sBE $1 $2 $ AccountExtraAssertion $2 }
  | tok_plus extra_tag { sBE $1 $2 $ AccountExtraTag $2 }

extra_account_assertion
  :: { LAccountAssertion }
  : tok_assert tok_currency currency_symbol { sBE $1 $3 $ AccountAssertionCurrency $3 }
  | tok_assert tok_no_virtual { sBE $1 $2 $ AccountAssertionVirtual (sL1 $2 AccountAssertionNoVirtual) }
  | tok_assert tok_virtual_allowed { sBE $1 $2 $ AccountAssertionVirtual (sL1 $2 AccountAssertionVirtualAllowed) }
  | tok_assert tok_virtual_only { sBE $1 $2 $ AccountAssertionVirtual (sL1 $2 AccountAssertionVirtualOnly) }

account_type
  :: { Located AccountType }
  : tok_var {% parseAccountType $1 }

tag_dec
  :: { LTagDeclaration }
  : tok_tag tag { sBE $1 $2 $ TagDeclaration $2 }

price_dec
  :: { LPriceDeclaration }
  : tok_price timestamp currency_symbol cost_exp { sBE $1 $4 $ PriceDeclaration $2 $3 $4 }

conversion_rate
  :: { LRationalExpression }
  : rational_exp { $1 }

transaction_dec
  :: { LTransaction }
  : timestamp transaction_lines
      { mkTransaction $1 $2 }

timestamp
  :: { Located Timestamp }
  : tok_timestamp {% parseTimestamp $1 }

-- The lines below the timestamp line of a transaction.
--
-- Note: This is one chain of productions rather than three lists, because the
-- comments above a line have to be read before it is known which kind of line
-- follows them.  With a list per kind of line, the parser would have to decide
-- which list a comment belongs to before reading past it, and an arbitrary
-- number of comments can stand between it and the line that decides:
-- descriptions/postings and postings/extras are both reduce/reduce conflicts
-- that way.  Chaining keeps the comments in one place per parser state.
transaction_lines
  :: { TransactionLines SourceSpan }
  : indented_comments {% noTrailingComments noTransactionLines $1 }
  | indented_comments some(description) postings_and_extras
      { let (postings, extras) = $3
         in TransactionLines
              (Just (Commented (combineDescriptions $2) $1))
              postings
              extras
      }
  | indented_comments posting postings_and_extras
      { let (postings, extras) = $3
         in TransactionLines Nothing (Commented $2 $1 : postings) extras
      }
  | indented_comments transaction_extra transaction_extras
      { TransactionLines Nothing [] (Commented $2 $1 : $3) }

postings_and_extras
  :: { ([Commented SourceSpan (Posting SourceSpan)], [Commented SourceSpan (TransactionExtra SourceSpan)]) }
  : indented_comments {% noTrailingComments ([], []) $1 }
  | indented_comments posting postings_and_extras
      { let (postings, extras) = $3
         in (Commented $2 $1 : postings, extras)
      }
  | indented_comments transaction_extra transaction_extras
      { ([], Commented $2 $1 : $3) }

-- TODO get the location of the pipe char in there too.
description
  :: { Located Description }
  : tok_pipe tok_anyline {% mapM (eitherParser "Description" Description.fromText) (parseAnyLine $2) }

posting
  :: { LPosting }
  : posting_header account_name account_exp currency_symbol optional(posting_price) optional(posting_ratio) { sBEMM $1 $4 $5 $6 $ Posting (locatedValue $1) $2 $3 $4 $5 $6 }

posting_header
  :: { Located Bool }
  : tok_star { sL1 $1 True }
  | tok_bang { sL1 $1 False }

posting_price
  :: { LPriceAnnotation }
  : tok_at cost_exp { sBE $1 $2 $ PriceAnnotationCost $2 }
  | lot_exp { sL1 $1 $ PriceAnnotationLot $1 }

lot_exp
  :: { LCostExpression }
  : tok_lot tok_at cost_exp { sBE $1 $3 (locatedValue $3) }

posting_ratio
  :: { LRatioExpression }
  : tok_tilde rational_exp {
        let (mInclusive, mRounding) = parseTilde $1
        in sBE $1 $2 $ RatioExpression mInclusive mRounding $2
      }

rational_exp
  :: { LRationalExpression }
  : tok_decimal_literal tok_slash tok_decimal_literal optional(tok_percent) { sBEM $1 $3 $4 $ RationalExpression (parseDecimalLiteral $1) (Just (parseDecimalLiteral $3)) (isJust $4) }
  | tok_decimal_literal optional(tok_percent) { sBM $1 $2 $ RationalExpression (parseDecimalLiteral $1) Nothing (isJust $2)}

account_name
  :: { Located AccountName }
  : tok_var {% parseAccountName $1 }

account_exp
  :: { Located DecimalLiteral }
  : tok_decimal_literal { parseDecimalLiteral $1 }

cost_exp
  :: { LCostExpression }
  : conversion_rate currency_symbol { sBE $1 $2 $ CostExpression $1 $2 }

transaction_extras
  :: { [Commented SourceSpan (TransactionExtra SourceSpan)] }
  : indented_comments {% noTrailingComments [] $1 }
  | indented_comments transaction_extra transaction_extras
      { Commented $2 $1 : $3 }

transaction_extra
  :: { LTransactionExtra }
  : tok_plus extra_attachment { sBE $1 $2 $ TransactionAttachment $2 }
  | tok_plus extra_assertion { sBE $1 $2 $ TransactionAssertion $2 }
  | tok_plus extra_tag { sBE $1 $2 $ TransactionTag $2 }

extra_attachment
  :: { LExtraAttachment }
  : tok_attach attachment { sBE $1 $2 $ ExtraAttachment $2 }

attachment
  :: { LAttachment }
  : rel_file_exp { sL1 $1 $ Attachment $1 }

-- Note: The scope is only known from the keyword, so each keyword builds the
-- assertion itself rather than fixing up one built without it.
extra_assertion
  :: { LExtraAssertion }
  : tok_assert account_name tok_eq account_exp commodity_exp
      { sBE $1 $5 $ ExtraAssertion (sBE $2 $5 $ AssertionEquals AssertionScopeReal $2 $4 $5) }
  | tok_assert_virtual account_name tok_eq account_exp commodity_exp
      { sBE $1 $5 $ ExtraAssertion (sBE $2 $5 $ AssertionEquals AssertionScopeVirtual $2 $4 $5) }

commodity_exp
  :: { LCommodityExpression }
  : currency_symbol { sL1 $1 $ CommodityExpressionCurrency $1 }
  | currency_symbol lot_exp { sBE $1 $2 $ CommodityExpressionLot $1 $2 }

extra_tag
  :: { LExtraTag }
  : tok_tag tag { sBE $1 $2 $ ExtraTag $2 }

tag
  :: { LTag }
  : tok_var {% parseTag $1 }

rel_file_exp
  :: { Located (Path Rel File) }
  : file_path_exp {% traverse (maybeParser "RelFile" parseRelFile) $1 }

file_path_exp
  :: { Located FilePath }
  : tok_file_path  { parseFilePath $1 }

-- Helpers
optional(p)
  :   { Nothing }
  | p { Just $1 }

-- list
many(p)
  : many_rev(p) { reverse $1 }

many_rev(p)
  : {- empty -}   { [] }
  | many_rev(p) p { $2 : $1 }

many_sep(sep, p)
  : many_sep_rev(sep, p) { reverse $1 }

many_sep_rev(sep, p)
  : {- empty -}   { [] }
  | many_rev(p) sep p { $3 : $1 }

-- Nonempty list
some(p)
  : some_rev(p) { NE.reverse $1 }

some_rev(p)
  : p             { $1 :| [] }
  | some_rev(p) p { $2 NE.<| $1 }

some_sep(sep, p)
  : some_sep_rev(sep, p) { NE.reverse $1 }

some_sep_rev(sep, p)
  : p                          { $1 :| [] }
  | some_sep_rev(sep, p) sep p { $3 NE.<| $1 }

{ 
{-# ANN module ("NOCOVER" :: String) #-}

sL1 :: Located a -> b -> Located b
sL1 (Located l val) b = Located l b

sBE :: Located a -> Located b -> c -> Located c
sBE (Located begin _) (Located end _) c = Located (combineSpans begin end) c

sBEM :: Located a -> Located b -> Maybe (Located c) -> d -> Located d
sBEM l1 l2 Nothing   = sBE l1 l2
sBEM l1 _  (Just l3) = sBE l1 l3

sBEML :: Located a -> Located b -> Maybe (Located c) -> [Located d] -> e -> Located e
sBEML l1 l2 mL3 []   = sBEM l1 l2 mL3
sBEML l1 _  _   l4   = sBL l1 l4

sBEMM :: Located a -> Located b -> Maybe (Located c) -> Maybe (Located d) -> e -> Located e
sBEMM l1 l2 mL3 Nothing   = sBEM l1 l2 mL3
sBEMM l1 _  _   (Just l4) = sBE l1 l4

sBL :: Located a -> [Located b] -> c -> Located c
sBL l1 [] = sL1 l1
sBL l1 ls = sBE l1 (last ls)

sBM :: Located a -> Maybe (Located c) -> f -> Located f
sBM l1 Nothing   = sL1 l1
sBM l1 (Just l3) = sBE l1 l3

sBML :: Located a -> Maybe (Located c) -> [Located d] -> f -> Located f
sBML l1 ml3 [] = sBM l1 ml3
sBML l1 _   ls = sBL l1 ls

sBMLL :: Located a -> Maybe (Located c) -> [Located d] -> [Located e] -> f -> Located f
sBMLL l1 ml3 l4 [] = sBML l1 ml3 l4
sBMLL l1 _ _ l5 = sBL l1 l5


parseTimestamp :: Token -> Alex (Located Timestamp)
parseTimestamp t@(Located _ (TokenTimestamp ds)) = sL1 t <$> eitherParser "Timestamp" Timestamp.fromString ds

parseAnyLine :: Token -> Located Text
parseAnyLine t@(Located _ (TokenAnyLine text)) = sL1 t text

-- | The lines of a transaction below its timestamp line.
data TransactionLines ann = TransactionLines
  { transactionLinesDescription :: Maybe (Commented ann Description)
  , transactionLinesPostings :: [Commented ann (Posting ann)]
  , transactionLinesExtras :: [Commented ann (TransactionExtra ann)]
  }

noTransactionLines :: TransactionLines ann
noTransactionLines = TransactionLines Nothing [] []

-- A comment on a line of its own belongs to the line below it, so comments left
-- over at the end of a block belong to no line at all.
noTrailingComments :: a -> Maybe (Located Comment) -> Alex a
noTrailingComments emptyValue = \case
  Nothing -> pure emptyValue
  Just (Located l _) ->
    alexError' l "this comment has no line below it to belong to"

mkTransaction ::
  Located Timestamp ->
  TransactionLines SourceSpan ->
  LTransaction
mkTransaction lTimestamp TransactionLines {..} =
  sBMLL
    lTimestamp
    (fmap commentedValue transactionLinesDescription)
    (map commentedValue transactionLinesPostings)
    (map commentedValue transactionLinesExtras)
    Transaction
      { transactionTimestamp = lTimestamp
      , transactionDescription = transactionLinesDescription
      , transactionPostings = transactionLinesPostings
      , transactionExtras = transactionLinesExtras
      }

parseComment :: Token -> Located Comment
parseComment t = sL1 t $ Comment $ case locatedValue t of
  TokenComment text -> text
  TokenIndentedComment text -> text

-- | A run of comment lines is one comment, of one line each.
combineComments :: [Located Comment] -> Maybe (Located Comment)
combineComments = \case
  [] -> Nothing
  (c : cs) -> Just $ sBL c cs $ Comment $ T.intercalate "\n" $ map (unComment . locatedValue) (c : cs)

commentDeclarations :: Maybe (Located Comment) -> [LDeclaration]
commentDeclarations = \case
  Nothing -> []
  Just lc -> [sL1 lc (DeclarationComment lc)]

-- Note: 'some' guarantees the list is not empty, which the type does not.
requireComments :: [Located Comment] -> Alex (Located Comment)
requireComments cs = case combineComments cs of
  Nothing -> alexError "Empty run of comment lines"
  Just lc -> pure lc

combineDescriptions :: NonEmpty (Located Description) -> Located Description
combineDescriptions dss@(d:|ds) = sBL d ds $ sconcat (NE.map locatedValue dss)

parseAccountName :: Token -> Alex (Located AccountName)
parseAccountName t@(Located _ (TokenVar ans)) = sL1 t <$> maybeParser "AccountName" AccountName.fromText ans

parseCurrencySymbol :: Token -> Alex (Located CurrencySymbol)
parseCurrencySymbol t@(Located _ (TokenVar ans)) = sL1 t <$> eitherParser "CurrencySymbol" CurrencySymbol.fromText ans

parseAccountType :: Token -> Alex (Located AccountType)
parseAccountType t@(Located _ (TokenVar ats)) = sL1 t <$> maybeParser "AccountType" AccountType.fromText ats

parseTag :: Token -> Alex (Located Tag)
parseTag t@(Located _ (TokenVar ans)) = sL1 t <$> eitherParser "Tag" Tag.fromText ans

parseTilde :: Token -> (Maybe Bool, Maybe Rounding)
parseTilde (Located _ (TokenTilde mInclusive mRounding)) = (mInclusive, mRounding)

parseDecimalLiteral :: Token -> Located DecimalLiteral
parseDecimalLiteral t@(Located _ (TokenDecimalLiteral dl)) = sL1 t dl

parseFilePath :: Token -> Located FilePath
parseFilePath t@(Located _ (TokenFilePath fp)) = sL1 t fp
  
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)
                                    
happyError :: Token -> Alex a
happyError (Located p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

parseModule :: Path Abs Dir -> Path Rel File -> Text -> Either String LModule
parseModule base fp = runAlex' moduleParser base fp . T.unpack

parseDeclaration :: Path Abs Dir -> Path Rel File -> Text -> Either String (Declaration SourceSpan)
parseDeclaration base fp = runAlex' (locatedValue <$> declarationParser) base fp . T.unpack

parseCurrencyDeclaration :: Path Abs Dir -> Path Rel File -> Text -> Either String (CurrencyDeclaration SourceSpan)
parseCurrencyDeclaration base fp = runAlex' (locatedValue <$> currencyDeclarationParser) base fp . T.unpack

parseAccountDeclaration :: Path Abs Dir -> Path Rel File -> Text -> Either String (AccountDeclaration SourceSpan)
parseAccountDeclaration base fp = runAlex' (locatedValue <$> accountDeclarationParser) base fp . T.unpack

parseTagDeclaration :: Path Abs Dir -> Path Rel File -> Text -> Either String (TagDeclaration SourceSpan)
parseTagDeclaration base fp = runAlex' (locatedValue <$> tagDeclarationParser) base fp . T.unpack

parsePriceDeclaration :: Path Abs Dir -> Path Rel File -> Text -> Either String (PriceDeclaration SourceSpan)
parsePriceDeclaration base fp = runAlex' (locatedValue <$> priceDeclarationParser) base fp . T.unpack

parseTransaction :: Path Abs Dir -> Path Rel File -> Text -> Either String (Transaction SourceSpan)
parseTransaction base fp = runAlex' (locatedValue <$> transactionParser) base fp . T.unpack
}
