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
%name importParser import_dec
%name transactionParser transaction_dec
%name postingParser posting
%name accountNameParser account_name
%name accountParser account_exp

%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Located _ TokenEOF }
-- Without this we get a type error
%error { happyError }

-- Don't allow conflicts
%expect 0

%token 
      import          { Located _ (TokenImport _)}
      comment         { Located _ (TokenComment _) }
      attach          { Located _ TokenAttach }
      assert          { Located _ TokenAssert }
      price           { Located _ TokenPrice }
      file_path       { Located _ (TokenFilePath _) }
      eq              { Located _ TokenEq }
      timestamp_tok   { Located _ (TokenTimestamp _) }
      var             { Located _ (TokenVar _) }
      pipetext        { Located _ (TokenDescription _) }
      decimal_literal { Located _ (TokenDecimalLiteral _) }
      plus            { Located _ TokenPlus }
      star            { Located _ TokenStar }
      bang            { Located _ TokenBang }
      at              { Located _ TokenAt }
      tilde           { Located _ TokenTilde }
      percent         { Located _ TokenPercent }
      currency_tok    { Located _ TokenCurrency}
      account_tok     { Located _ TokenAccount }
      newline         { Located _ TokenNewLine }


%%

module
  :: { LModule }
  : many(newline) many(import_with_newlines) many(declaration_with_newlines) { Module $2 $3 }

import_with_newlines
  :: { Located Import }
  : import_dec many(newline) { $1 }

import_dec
  :: { Located Import }
  : import {% parseImport $1 }


declaration_with_newlines
  :: { LDeclaration }
  : declaration many(newline) { $1 }

declaration
  :: { LDeclaration }
  : comment_dec { DeclarationComment $1 }
  | currency_dec { DeclarationCurrency $1 }
  | account_dec { DeclarationAccount $1 }
  | price_dec { DeclarationPrice $1 }
  | transaction_dec { DeclarationTransaction $1 }

comment_dec
  :: { Located Text }
  : comment { parseComment $1 }

currency_dec
  :: { LCurrencyDeclaration }
  : currency_tok currency_symbol quantisation_factor newline { sBE $1 $4 $ CurrencyDeclaration $2 $3 }

currency_symbol
  :: { Located CurrencySymbol }
  : var {% parseCurrencySymbol $1 }

quantisation_factor
  :: { Located DecimalLiteral }
  : decimal_literal { parseDecimalLiteral $1 }

account_dec
  :: { LAccountDeclaration }
  : account_tok account_name optional(account_type) newline { sBE $1 $4 $ AccountDeclaration $2 $3 }

account_type
  :: { Located AccountType }
  : var {% parseAccountType $1 }

price_dec
  :: { LPriceDeclaration }
  : price timestamp currency_symbol cost_exp newline { sBE $1 $5 $ PriceDeclaration $2 $3 $4 }

conversion_rate
  :: { Located DecimalLiteral }
  : decimal_literal { parseDecimalLiteral $1 }

transaction_dec
  :: { LTransaction }
  : timestamp newline descriptions postings transaction_extras { sBMLL $1 $2 $3 $4 $5 (Transaction $1 $3 $4 $5) }
  | timestamp %shift { sL1 $1 $ Transaction $1 Nothing [] [] }

timestamp
  :: { Located Timestamp }
  : timestamp_tok {% parseTimestamp $1 }

descriptions
  :: { Maybe (Located Description) }
  : many(description) { combineDescriptions $1 }

description
  :: { Located Description }
  : pipetext {% parseDescription $1 }

postings
  :: { [LPosting] }
  : many(posting) { $1 }

posting
  :: { LPosting }
  : posting_header account_name account_exp currency_symbol optional(posting_cost) optional(posting_percentage) newline { sBE $1 $7 $ Posting (locatedValue $1) $2 $3 $4 $5 $6 }

posting_header
  :: { Located Bool }
  : star { sL1 $1 True }
  | bang { sL1 $1 False }

posting_cost
  :: { LCostExpression }
  : at cost_exp { $2 }

posting_percentage
  :: { LPercentageExpression }
  : tilde percentage percent { sBE $1 $3 $ PercentageExpression $2 }

percentage
  :: { Located DecimalLiteral }
  : decimal_literal { parseDecimalLiteral $1 }

account_name
  :: { Located AccountName }
  : var {% parseAccountName $1 }

account_exp
  :: { Located DecimalLiteral }
  : decimal_literal { parseDecimalLiteral $1 }

cost_exp
  :: { LCostExpression }
  : conversion_rate currency_symbol { sBE $1 $2 $ CostExpression $1 $2 }

transaction_extras
  :: { [LTransactionExtra] }
  : many(transaction_extra) { $1 }

transaction_extra
  :: { LTransactionExtra }
  : plus attachment { sBE $1 $2 $ TransactionAttachment $2 }
  | plus assertion { sBE $1 $2 $ TransactionAssertion $2 }

attachment
  :: { LAttachment }
  : attach rel_file_exp newline { sBE $1 $3 $ Attachment $2 }

assertion
  :: { LAssertion }
  : assert account_name eq account_exp currency_symbol newline { sBE $1 $6 $ AssertionEquals $2 $4 $5 }

rel_file_exp
  :: { Located (Path Rel File) }
  : file_path_exp {% traverse (maybeParser "RelFile" parseRelFile) $1 }

file_path_exp
  :: { Located FilePath }
  : file_path  { parseFilePath $1 }


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

sBME :: Located a -> Maybe (Located b) -> c -> Located c
sBME l1 Nothing = sL1 l1
sBME l1 (Just l2) = sBE l1 l2

sBL :: Located a -> [Located b] -> c -> Located c
sBL l1 [] = sL1 l1
sBL l1 ls = sBE l1 (last ls)

sBMLL :: Located a -> Located b -> Maybe (Located c) -> [Located d] -> [Located e] -> f -> Located f
sBMLL l1 l2 Nothing  [] [] = sBE l1 l2
sBMLL l1 _ (Just l3) [] [] = sBE l1 l3
sBMLL l1 _ _         ls [] = sBL l1 ls
sBMLL l1 _ _         _  ls = sBL l1 ls

parseImport :: Token -> Alex (Located Import)
parseImport t@(Located _ (TokenImport s)) = sL1 t <$> parseImportFrom s

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
parseCurrencySymbol t@(Located _ (TokenVar ans)) = sL1 t <$> eitherParser "CurrencySymbol" (CurrencySymbol.fromText) ans

parseAccountType :: Token -> Alex (Located AccountType)
parseAccountType t@(Located _ (TokenVar ats)) = sL1 t <$> maybeParser "AccountType" (AccountType.fromText) ats

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
