{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}
module Centjes.Happy
  ( parseModule
  , parseDeclaration
  , parseTransaction
  , parsePosting
  , parseAccountName
  , parseAccount
  , parseAmount
  ) where

import Centjes.Alex
import Centjes.Module
import Data.List
import Data.Text (Text)
import Data.Time
import Debug.Trace
import Money.Amount as Money (Amount)
import qualified Money.Amount as Amount
import Money.Account as Money (Account)
import qualified Money.Account as Account
import qualified Data.Text as T

}

-- GHC's Happy file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y

%name moduleParser module
%name declarationParser declaration
%name transactionParser transaction
%name postingParser posting
%name accountNameParser account_name
%name accountParser account
%name amountParser amount

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
      string          { Token _ (TokenString $$) }
      int             { Token _ (TokenInt $$) }
      emptyline       { Token _ TokenEmptyLine }
      newline         { Token _ TokenNewLine }


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
  : timestamp newline manySep(newline, posting) { Transaction $1 $3 }
  | timestamp { Transaction $1 [] }

timestamp
  :: { Timestamp }
  : day {% timeParser "%F" $1 }

posting
  :: { Posting }
  : account_name account { Posting $1 $2 }

account_name
  :: { AccountName }
  : string { AccountName $1 } -- TODO do actual paring

account
  :: { Money.Account }
  : int {% maybeParser "account" Account.fromMinimalQuantisations $1 }

amount
  :: { Money.Amount }
  : int { Amount.fromMinimalQuantisations (fromIntegral $1) } -- TODO fromIntegral is wrong.

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
  | {- empty -} { [] }

manySep_rev(sep, p)
  : manySep_rev(sep, p) sep p { $3 : $1 }
  | p { [$1] }                      
{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)
                                    
happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

timeParser :: ParseTime t => String -> String -> Alex t
timeParser formatString s = case parseTimeM False defaultTimeLocale formatString s of
  Nothing -> parseError $ "Failed to parse time value: " <> formatString
  Just t -> pure t

maybeParser :: Show b => String -> (b -> Maybe a) -> b -> Alex a
maybeParser name func b = 
  case func b of
    Nothing -> parseError $ "Failed to parse " <> name <> " from " <> show b
    Just a -> pure a

parseError = alexError

parseModule :: FilePath -> Text -> Either String Module
parseModule fp = runAlex' moduleParser fp . T.unpack

parseDeclaration :: FilePath -> Text -> Either String Declaration
parseDeclaration fp = runAlex' declarationParser fp . T.unpack

parseTransaction :: FilePath -> Text -> Either String Transaction
parseTransaction fp = runAlex' transactionParser fp . T.unpack

parsePosting :: FilePath -> Text -> Either String Posting
parsePosting fp = runAlex' postingParser fp . T.unpack

parseAccountName :: FilePath -> Text -> Either String AccountName
parseAccountName fp = runAlex' accountNameParser fp . T.unpack

parseAccount :: FilePath -> Text -> Either String Money.Account
parseAccount fp = runAlex' accountParser fp . T.unpack

parseAmount :: FilePath -> Text -> Either String Money.Amount
parseAmount fp = runAlex' amountParser fp . T.unpack
}
