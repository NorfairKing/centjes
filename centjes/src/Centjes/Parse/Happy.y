{
{-# LANGUAGE OverloadedStrings #-}
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
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Description as Description
import Centjes.Location
import Centjes.Module
import Centjes.Parse.Alex
import Centjes.Parse.Utils
import Centjes.Tag as Tag
import Centjes.Timestamp as Timestamp
import Data.Text (Text)
import Numeric.DecimalLiteral (DecimalLiteral)
import Path
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup

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
-- %expect 0

%token 
      tok_import          { Located _ TokenImport }
      tok_comment         { Located _ (TokenComment _) }
      tok_attach          { Located _ TokenAttach }
      tok_assert          { Located _ TokenAssert }
      tok_tag             { Located _ TokenTag }
      tok_price           { Located _ TokenPrice }
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
      tok_tilde           { Located _ TokenTilde }
      tok_percent         { Located _ TokenPercent }
      tok_currency        { Located _ TokenCurrency}
      tok_account         { Located _ TokenAccount }
      tok_newline         { Located _ TokenNewLine }


%%

module
  :: { LModule }
  : newlines many_sep(some(tok_newline), import_dec) many_sep(some(tok_newline), declaration) { Module $2 $3 }

import_with_newlines
  :: { LImport }
  : import_dec newlines { $1 }

import_dec
  :: { LImport }
  : tok_import rel_file_exp { sBE $1 $2 $ Import $2 }

declaration
  :: { LDeclaration }
  : comment_dec { sL1 $1 $ DeclarationComment $1 }
  | currency_dec { sL1 $1 $ DeclarationCurrency $1 }
  | account_dec { sL1 $1 $ DeclarationAccount $1 }
  | tag_dec { sL1 $1 $ DeclarationTag $1 }
  | price_dec { sL1 $1 $ DeclarationPrice $1 }
  | transaction_dec { sL1 $1 $ DeclarationTransaction $1 }

comment_dec
  :: { Located Text }
  : tok_comment { parseComment $1 }

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
  : tok_account account_name { sBE $1 $2 $ AccountDeclaration $2 Nothing }
  | tok_account account_name account_type { sBE $1 $3 $ AccountDeclaration $2 (Just $3) }

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
  : timestamp tok_newline descriptions { sBE $1 $3 $ Transaction $1 (Just $3) [] [] }
  | timestamp %shift { sL1 $1 $ Transaction $1 Nothing [] [] }

-- : timestamp tok_newline descriptions postings transaction_extras { sBMLL $1 $2 $3 $4 $5 (Transaction $1 $3 $4 $5) }
-- | timestamp %shift { sL1 $1 $ Transaction $1 Nothing [] [] }

timestamp
  :: { Located Timestamp }
  : tok_timestamp {% parseTimestamp $1 }

descriptions
  :: { Located Description }
  : some_sep(tok_newline, description) { combineDescriptions $1 }

-- TODO get the location of the pipe char in there too.
description
  :: { Located Description }
  : tok_pipe tok_anyline {% mapM (eitherParser "Description" Description.fromText) (parseAnyLine $2) }

postings
  :: { [LPosting] }
  : many_sep(tok_newline, posting) { $1 }

posting
  :: { LPosting }
  : posting_header account_name account_exp currency_symbol optional(posting_cost) posting_percentage { sBE $1 $6 $ Posting (locatedValue $1) $2 $3 $4 $5 (Just $6) }
  | posting_header account_name account_exp currency_symbol posting_cost %shift { sBE $1 $5 $ Posting (locatedValue $1) $2 $3 $4 (Just $5) Nothing }
  | posting_header account_name account_exp currency_symbol %shift { sBE $1 $4 $ Posting (locatedValue $1) $2 $3 $4 Nothing Nothing }

posting_header
  :: { Located Bool }
  : tok_star { sL1 $1 True }
  | tok_bang { sL1 $1 False }

posting_cost
  :: { LCostExpression }
  : tok_at cost_exp { $2 }

posting_percentage
  :: { LPercentageExpression }
  : tok_tilde rational_exp tok_percent { sBE $1 $3 $ PercentageExpression $2 }

rational_exp
  :: { LRationalExpression }
  : tok_decimal_literal { sL1 $1 $ RationalExpressionDecimal $ parseDecimalLiteral $1 }
  | tok_decimal_literal tok_slash tok_decimal_literal { sBE $1 $3 $ RationalExpressionFraction (parseDecimalLiteral $1) (parseDecimalLiteral $3) }

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
  :: { [LTransactionExtra] }
  : many_sep(tok_newline, transaction_extra) { $1 }

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

extra_assertion
  :: { LExtraAssertion }
  : tok_assert assertion { sBE $1 $2 $ ExtraAssertion $2 }

assertion
  :: { LAssertion }
  : account_name tok_eq account_exp currency_symbol { sBE $1 $4 $ AssertionEquals $1 $3 $4 }

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

newlines
  :: { [Token] }
  : many(tok_newline) { $1 }

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

sL1 :: Located a -> b -> Located b
sL1 (Located l val) b = Located l b

sBE :: Located a -> Located b -> c -> Located c
sBE (Located begin _) (Located end _) c = Located (combineSpans begin end) c

sBL :: Located a -> [Located b] -> c -> Located c
sBL l1 [] = sL1 l1
sBL l1 ls = sBE l1 (last ls)

sBM :: Located a -> Located b -> Maybe (Located c) -> f -> Located f
sBM l1 l2 Nothing  = sBE l1 l2
sBM l1 _ (Just l3) = sBE l1 l3

sBML :: Located a -> Located b -> Maybe (Located c) -> [Located d] -> f -> Located f
sBML l1 l2 ml3 [] = sBM l1 l2 ml3
sBML l1 _ _    ls = sBL l1 ls

sBMLL :: Located a -> Located b -> Maybe (Located c) -> [Located d] -> [Located e] -> f -> Located f
sBMLL l1 l2 ml3 l4 [] = sBML l1 l2 ml3 l4
sBMLL l1 _ _ _ l5 = sBL l1 l5

parseComment :: Token -> Located Text
parseComment (Located l (TokenComment t)) = Located l t

parseTimestamp :: Token -> Alex (Located Timestamp)
parseTimestamp t@(Located _ (TokenTimestamp ds)) = sL1 t <$> eitherParser "Timestamp" Timestamp.fromString ds

parseAnyLine :: Token -> Located Text
parseAnyLine t@(Located _ (TokenAnyLine text)) = sL1 t text

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
