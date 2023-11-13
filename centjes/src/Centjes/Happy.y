{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}
module Centjes.Happy
  ( parseModule
  , parseDeclaration
  , parseTransaction
  , parsePosting
  ) where

import Centjes.Alex
import Centjes.Module
import Data.List
import Data.Text(Text)
import Debug.Trace
import Money.Account as Money (Account)
import qualified Data.Text as T

}

-- GHC's Happy file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y

%name moduleParser module
%name declarationParser declaration
%name transactionParser transaction
%name postingParser posting

%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

-- Don't allow conflicts
%expect 0

%token 
      comment         { Token _ (TokenComment $$) }
      string          { Token _ (TokenString $$) }
      int             { Token _ (TokenInt $$) }
      float           { Token _ (TokenFloat $$) }
      emptyline       { Token _ TokenEmptyLine }


%right arrow
%nonassoc '='

%%

module
  :: { Module }
  : manySep(emptyline, declaration) { Module $1 }

declaration
  :: { Declaration }
  : transaction { DeclarationTransaction $1 }

transaction
  :: { Transaction }
  : timestamp many(posting) { Transaction $1 $2 }

posting
  :: { Posting }
  : account_name account { Posting $1 $2 }

timestamp
  :: { Timestamp }
  : string { undefined }

account_name
  :: { AccountName }
  : string { $1 }

account
  :: { Money.Account }
  : int { undefined }

-- Helpers
-- Nonempty list with separator
optional(p)
  :   { Nothing }
  | p { Just $1 }

many_rev(p)
  : {- empty -}   { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

-- list with separator
manySep(sep, p)
  : manySep_rev(sep, p) { reverse $1 }

manySep_rev(sep, p)
  : manySep_rev(sep, p) sep p { $3 : $1 }
  | p { [$1] }                      
  | {- empty -} { [] }
{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)
                                    
happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseError = alexError

parseModule :: FilePath -> Text -> Either String Module
parseModule fp = runAlex' moduleParser fp . T.unpack

parseDeclaration :: FilePath -> Text -> Either String Declaration
parseDeclaration fp = runAlex' declarationParser fp . T.unpack

parseTransaction :: FilePath -> Text -> Either String Transaction
parseTransaction fp = runAlex' transactionParser fp . T.unpack

parsePosting :: FilePath -> Text -> Either String Posting
parsePosting fp = runAlex' postingParser fp . T.unpack
}
