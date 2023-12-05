{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}
module Centjes.Parse.Happy
  ( parseModule
  , parseDeclaration
  , parseTransaction
  ) where

import Centjes.AccountName as AccountName
import Centjes.Description as Description
import Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Location
import Centjes.Module
import Centjes.Parse.Alex
import Centjes.Parse.Utils
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
      comment         { Located _ (TokenComment _) }
      attach          { Located _ (TokenAttachment _) }
      day             { Located _ (TokenDay _) }
      var             { Located _ (TokenVar _) }
      pipetext        { Located _ (TokenDescription _) }
      decimal_literal { Located _ (TokenDecimalLiteral _) }
      star            { Located _ TokenStar }
      dot             { Located _ TokenDot }
      import          { Located _ (TokenImport $$ )}
      currency_tok    { Located _ TokenCurrency}
      account_tok     { Located _ TokenAccount}
      newline         { Located _ TokenNewLine }


%%

module
  :: { LModule }
  : many(newline) many(import_with_newlines) many(declaration_with_newlines) { Module $2 $3 }

import_with_newlines
  :: { Import }
  : import_dec many(newline) { $1 }

import_dec
  :: { Import }
  : import {% parseImportFrom $1 }

declaration_with_newlines
  :: { LDeclaration }
  : declaration many(newline) { $1 }

declaration
  :: { LDeclaration }
  : comment_dec { DeclarationComment $1 }
  | currency_dec { DeclarationCurrency $1 }
  | account_dec { DeclarationAccount $1 }
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
  : account_tok account_name newline { sBE $1 $2 $ AccountDeclaration $2 }

transaction_dec
  :: { LTransaction }
  : timestamp newline optional(description) many(posting) many(attachment) { sBMLL $1 $3 $4 $5 (Transaction $1 $3 $4 $5) }
  | timestamp %shift { sL1 $1 $ Transaction $1 Nothing [] [] }

timestamp
  :: { Located Timestamp }
  : day {% parseTimestamp $1 }

description
  :: { Located Description }
  : pipetext {% parseDescription $1 }

posting
  :: { LPosting }
  : star account_name account_exp currency_symbol newline { sBE $1 $5 $ Posting $2 $3 $4 }

account_name
  :: { Located AccountName }
  : var {% parseAccountName $1 }

account_exp
  :: { Located DecimalLiteral }
  : decimal_literal { parseDecimalLiteral $1 }

attachment
  :: { Located Attachment }
  : attach newline {% parseAttachment $1 }

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

sBMLL :: Located a -> Maybe (Located b) -> [Located c] -> [Located d] -> e -> Located e
sBMLL l1 Nothing   [] [] = sL1 l1
sBMLL l1 (Just l2) [] [] = sBE l1 l2
sBMLL l1 _         ls [] = sBL l1 ls
sBMLL l1 _         _  ls = sBL l1 ls

parseComment :: Token -> Located Text
parseComment (Located l (TokenComment t)) = Located l t

parseTimestamp :: Token -> Alex (Located Timestamp)
parseTimestamp t@(Located _ (TokenDay ds)) = fmap Timestamp . sL1 t <$> timeParser "%F" ds

parseDescription :: Token -> Alex (Located Description)
parseDescription t@(Located _ (TokenDescription ds)) = sL1 t <$> maybeParser "Description" Description.fromText ds 

parseAccountName :: Token -> Alex (Located AccountName)
parseAccountName t@(Located _ (TokenVar ans)) = sL1 t <$> maybeParser "AccountName" AccountName.fromText ans

parseAttachment :: Token -> Alex (Located Attachment)
parseAttachment t@(Located _ (TokenAttachment atch)) = sL1 t <$> maybeParser "Attachment" (fmap Attachment . parseRelFile) atch

parseCurrencySymbol :: Token -> Alex (Located CurrencySymbol)
parseCurrencySymbol t@(Located _ (TokenVar ans)) = sL1 t <$> maybeParser "CurrencySymbol" (CurrencySymbol.fromText) ans

parseDecimalLiteral :: Token -> Located DecimalLiteral
parseDecimalLiteral t@(Located _ (TokenDecimalLiteral dl)) = sL1 t dl
  
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)
                                    
happyError :: Token -> Alex a
happyError (Located p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

parseModule :: FilePath -> Text -> Either String LModule
parseModule fp = runAlex' moduleParser fp . T.unpack

parseDeclaration :: FilePath -> Text -> Either String LDeclaration
parseDeclaration fp = runAlex' declarationParser fp . T.unpack

parseTransaction :: FilePath -> Text -> Either String (Transaction SourceSpan)
parseTransaction fp = runAlex' (locatedValue <$> transactionParser) fp . T.unpack
}
