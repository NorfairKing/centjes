{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}
module Centjes.Parse.Happy
  ( parseModule
  , parseDeclaration
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

}

-- GHC's Happy file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y

%name moduleParser module
%name declarationParser declaration
%name transactionParser transaction_dec

%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Located _ TokenEOF }
-- Without this we get a type error
%error { happyError }

-- Don't allow conflicts
%expect 0

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
      tok_pipetext        { Located _ (TokenDescription _) }
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
  : newlines many(import_with_newlines) many(declaration_with_newlines) { Module $2 $3 }

import_with_newlines
  :: { LImport }
  : import_dec newlines { $1 }

import_dec
  :: { LImport }
  : tok_import rel_file_exp tok_newline { sBE $1 $3 $ Import $2 }


declaration_with_newlines
  :: { LDeclaration }
  : declaration newlines { $1 }

declaration
  :: { LDeclaration }
  : comment_dec { DeclarationComment $1 }
  | currency_dec { DeclarationCurrency $1 }
  | account_dec { DeclarationAccount $1 }
  | tag_dec { DeclarationTag $1 }
  | price_dec { DeclarationPrice $1 }
  | transaction_dec { DeclarationTransaction $1 }

comment_dec
  :: { Located Text }
  : tok_comment { parseComment $1 }

currency_dec
  :: { LCurrencyDeclaration }
  : tok_currency currency_symbol quantisation_factor tok_newline { sBE $1 $4 $ CurrencyDeclaration $2 $3 }

currency_symbol
  :: { Located CurrencySymbol }
  : tok_var {% parseCurrencySymbol $1 }

quantisation_factor
  :: { Located DecimalLiteral }
  : tok_decimal_literal { parseDecimalLiteral $1 }

account_dec
  :: { LAccountDeclaration }
  : tok_account account_name optional(account_type) tok_newline { sBE $1 $4 $ AccountDeclaration $2 $3 }

account_type
  :: { Located AccountType }
  : tok_var {% parseAccountType $1 }

tag_dec
  :: { LTagDeclaration }
  : tok_tag tag tok_newline { sBE $1 $3 $ TagDeclaration $2 }

price_dec
  :: { LPriceDeclaration }
  : tok_price timestamp currency_symbol cost_exp tok_newline { sBE $1 $5 $ PriceDeclaration $2 $3 $4 }

conversion_rate
  :: { LRationalExpression }
  : rational_exp { $1 }

transaction_dec
  :: { LTransaction }
  : timestamp tok_newline descriptions postings transaction_extras { sBMLL $1 $2 $3 $4 $5 (Transaction $1 $3 $4 $5) }
  | timestamp %shift { sL1 $1 $ Transaction $1 Nothing [] [] }

timestamp
  :: { Located Timestamp }
  : tok_timestamp {% parseTimestamp $1 }

descriptions
  :: { Maybe (Located Description) }
  : many(description) { combineDescriptions $1 }

description
  :: { Located Description }
  : tok_pipetext {% parseDescription $1 }

postings
  :: { [LPosting] }
  : many(posting) { $1 }

posting
  :: { LPosting }
  : posting_header account_name account_exp currency_symbol optional(posting_cost) optional(posting_percentage) tok_newline { sBE $1 $7 $ Posting (locatedValue $1) $2 $3 $4 $5 $6 }

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
  : many(transaction_extra) { $1 }

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
  : rel_file_exp tok_newline { sBE $1 $2 $ Attachment $1 }

extra_assertion
  :: { LExtraAssertion }
  : tok_assert assertion { sBE $1 $2 $ ExtraAssertion $2 }

assertion
  :: { LAssertion }
  : account_name tok_eq account_exp currency_symbol tok_newline { sBE $1 $5 $ AssertionEquals $1 $3 $4 }

extra_tag
  :: { LExtraTag }
  : tok_tag tag tok_newline { sBE $1 $3 $ ExtraTag $2 }

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

{ 

sL1 :: Located a -> b -> Located b
sL1 (Located l val) b = Located l b

sBE :: Located a -> Located b -> c -> Located c
sBE (Located begin _) (Located end _) c = Located (combineSpans begin end) c

sBL :: Located a -> [Located b] -> c -> Located c
sBL l1 [] = sL1 l1
sBL l1 ls = sBE l1 (last ls)

sBMLL :: Located a -> Located b -> Maybe (Located c) -> [Located d] -> [Located e] -> f -> Located f
sBMLL l1 l2 Nothing  [] [] = sBE l1 l2
sBMLL l1 _ (Just l3) [] [] = sBE l1 l3
sBMLL l1 _ _         ls [] = sBL l1 ls
sBMLL l1 _ _         _  ls = sBL l1 ls

parseComment :: Token -> Located Text
parseComment (Located l (TokenComment t)) = Located l t

parseTimestamp :: Token -> Alex (Located Timestamp)
parseTimestamp t@(Located _ (TokenTimestamp ds)) = sL1 t <$> eitherParser "Timestamp" Timestamp.fromString ds

parseDescription :: Token -> Alex (Located Description)
parseDescription t@(Located _ (TokenDescription ds)) = sL1 t <$> eitherParser "Description" Description.fromText ds 

combineDescriptions :: [Located Description] -> Maybe (Located Description)
combineDescriptions [] = Nothing
combineDescriptions dss@(d:ds) = sBL d ds <$> Description.combine (map locatedValue dss)

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

parseDeclaration :: Path Abs Dir -> Path Rel File -> Text -> Either String LDeclaration
parseDeclaration base fp = runAlex' declarationParser base fp . T.unpack

parseTransaction :: Path Abs Dir -> Path Rel File -> Text -> Either String (Transaction SourceSpan)
parseTransaction base fp = runAlex' (locatedValue <$> transactionParser) base fp . T.unpack
}
