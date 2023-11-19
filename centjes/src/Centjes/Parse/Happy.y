{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}
module Centjes.Parse.Happy
  ( parseModule
  , parseDeclaration
  , parseImport
  , parseTransaction
  , parsePosting
  , parseAccountName
  , parseAccount
  ) where

import Centjes.DecimalLiteral
import Centjes.Module
import Centjes.Parse.Alex
import Centjes.Parse.Utils
import Data.List
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time
import Debug.Trace
import Money.Account as Money (Account)
import Money.Amount as Money (Amount)
import Money.QuantisationFactor as Money
import Path (parseRelFile)
import Text.ParserCombinators.ReadP
import qualified Centjes.DecimalLiteral as DecimalLiteral
import qualified Data.Text as T
import qualified Money.Account as Account
import qualified Money.Amount as Amount

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
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

-- Don't allow conflicts
%expect 0

%token 
      comment         { Token _ (TokenComment $$) }
      day             { Token _ (TokenDay $$) }
      var             { Token _ (TokenVar $$) }
      pipetext        { Token _ (TokenDescription $$) }
      decimal_literal { Token _ (TokenDecimalLiteral $$) }
      star            { Token _ TokenStar }
      dot             { Token _ TokenDot }
      import          { Token _ (TokenImport $$ )}
      currency_tok    { Token _ TokenCurrency}
      account_tok     { Token _ TokenAccount}
      newline         { Token _ TokenNewLine }


%right arrow
%nonassoc '='

%%

module
  :: { Module }
  : many(newline) many(import_with_newlines) many(declaration_with_newlines) { Module $2 $3 }

import_with_newlines
  :: { Import }
  : import_dec many(newline) { $1 }

import_dec
  :: { Import }
  : import {% parseImportFrom $1 }

declaration_with_newlines
  :: { Declaration }
  : declaration many(newline) { $1 }

declaration
  :: { Declaration }
  : currency_dec { DeclarationCurrency $1 }
  | account_dec { DeclarationAccount $1 }
  | transaction_dec { DeclarationTransaction $1 }

currency_dec
  :: { CurrencyDeclaration }
  : currency_tok currency_symbol quantisation_factor newline { CurrencyDeclaration $2 $3 } -- TODO actual parsing

currency_symbol
  :: { CurrencySymbol }
  : var { CurrencySymbol $1 } -- TODO actual parsing

quantisation_factor
  :: { DecimalLiteral }
  : decimal_literal { $1 }

account_dec
  :: { AccountDeclaration }
  : account_tok account_name { AccountDeclaration $2 }

transaction_dec
  :: { Transaction }
  : timestamp newline description many(posting) { Transaction $1 $3 $4 }
  | timestamp %shift { Transaction $1 (Description "") [] }

timestamp
  :: { Timestamp }
  : day {% timeParser "%F" $1 }

description
  :: { Description }
  : pipetext { Description $1 } -- TODO actual parsing
  | {- empty -} { Description "" }

posting
  :: { Posting }
  : star account_name account_exp currency_symbol newline { Posting $2 $3 $4 }

account_name
  :: { AccountName }
  : var { AccountName $1 } -- TODO do actual paring

account_exp
  :: { DecimalLiteral }
  : decimal_literal { $1 } -- TODO do actual parsing

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
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)
                                    
happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ show t ++ "'")

parseModule :: FilePath -> Text -> Either String Module
parseModule fp = runAlex' moduleParser fp . T.unpack

parseDeclaration :: FilePath -> Text -> Either String Declaration
parseDeclaration fp = runAlex' declarationParser fp . T.unpack

parseImport :: FilePath -> Text -> Either String Import
parseImport fp = runAlex' importParser fp . T.unpack

parseTransaction :: FilePath -> Text -> Either String Transaction
parseTransaction fp = runAlex' transactionParser fp . T.unpack

parsePosting :: FilePath -> Text -> Either String Posting
parsePosting fp = runAlex' postingParser fp . T.unpack

parseAccountName :: FilePath -> Text -> Either String AccountName
parseAccountName fp = runAlex' accountNameParser fp . T.unpack

parseAccount :: FilePath -> Text -> Either String DecimalLiteral
parseAccount fp = runAlex' accountParser fp . T.unpack
}
