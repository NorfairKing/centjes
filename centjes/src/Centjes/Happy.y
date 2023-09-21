{
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -w #-}
module Centjes.Happy
  ( parseModule
  , parseExpression
  , parseType
  ) where

import Debug.Trace
import Data.List
import qualified Data.Text as T
import Centjes.Module

}

-- The expression language used here comes straight from the happy
-- documentation with virtually no changes (open, so TokenOB/TokenCB were
-- changed to TokenLParen/TokenRParen

-- GHC's Happy file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser.y

%name moduleParser module
%name declarationParser declaration
%name expressionParser expression
%name typeParser type
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

-- Don't allow conflicts
%expect 0

%token 
      let             { Token _ TokenLet }
      in              { Token _ TokenIn }

      comment         { Token _ (TokenComment $$) }

      var             { Token _ (TokenVar $$) }

      int             { Token _ (TokenInt $$) }
      float           { Token _ (TokenFloat $$) }

      arrow           { Token _ TokenArrow }
      '='             { Token _ TokenEq }
      '-'             { Token _ TokenMinus }
      column          { Token _ TokenColumn }
      lambda          { Token _ TokenBackslash }
      '('             { Token _ TokenLParen }
      ')'             { Token _ TokenRParen }

      layout_open     { Token _ TokenLayoutOpen }
      layout_delimit  { Token _ TokenLayoutDelimit }
      layout_close    { Token _ TokenLayoutClose }


%right arrow
%nonassoc '='

%%

module
  :: { Module }
  : declarations { Module $1 }

declarations
  :: { [Declaration] }
  : layed_out(declarations_delimited) { $1 }
  | {- empty -} { [] }

declarations_delimited
  :: { [Declaration] }
  : layed_out_delimited(declaration) { $1 }

layed_out_delimited(p)
  : p layout_delimit layed_out_delimited(p) { $1 : $3 }
  | p  { [$1] }
  | {- empty -} { [] }

delimited(p)
  : p layout_delimit delimited(p) { $1 : $3 }
  | p { [$1] }
  | {- empty -} { [] }

layed_out(p)
  : layout_open p close_layout { $2 }

close_layout
  : layout_close { () }
  | error {% popLayout } -- Parse errors can end layout blocks

declaration
  :: { Declaration }
  : declaration_comment identifier '=' expression { Declaration $1 $2 $4 }

declaration_comment
  :: { Maybe Comment }
  : declaration_comments { Just (T.pack (intercalate "\n" $1)) }
  | {- empty -} { Nothing }

declaration_comments
  :: { [String] }
  : comment layout_delimit declaration_comments { $1 : $3 }
  | comment layout_delimit { [$1] }

expression
  :: { Expression }
  : commentlines expression { ExpressionComment $1 $2 }
  | expression_without_comment %shift { $1 }

commentlines
  :: { Comment }
  : commentlines comment { $1 <> T.pack ('\n' : $2) }
  | comment %shift { T.pack $1 }

expression_without_comment :: { Expression }
  : expression_10 column type_atom { ExpressionTypeAnnotation $1 $3 }
  | expression_10 %shift { $1 }

expression_10
  :: { Expression }
  : lambda identifier arrow expression { ExpressionLambda $2 $4 }
  | let_expression { $1 }
  | expression_f %shift { $1 }

let_expression
  :: { Expression }
  : let layed_out(let_bind) in expression  { let (i, e) = $2 in ExpressionLet i e $4 }

let_bind 
  :: { (Identifier, Expression) }
  : identifier '=' expression { ($1, $3) }

expression_f
  :: { Expression }
  : expression_f commentlines expression_a { ExpressionApplication $1 (ExpressionComment $2 $3) }
  | expression_f expression_a { ExpressionApplication $1 $2 }
  | expression_a { $1 }

expression_a
  :: { Expression }
  : literal { ExpressionLiteral $1 }
  | identifier { ExpressionVariable $1 }
  | '(' expression ')' { $2 }

type
  :: { Type }
  : type arrow type_atom { TypeFunction $1 $3 }
  | type_atom { $1 }

type_atom
  :: { Type }
  : identifier { TypeConstructor $1 }
  | '(' type ')' { $2 }

identifier   :: { Identifier }
             : var { Identifier $1 }

literal      :: { Literal }
             : intLit { LiteralInteger $1 }
             | floatLit { LiteralFloat $1 }

intLit       :: { Integer }
             : int { $1 }
             | '-' int { (- $2) }

floatLit     :: { Double }
             : float { $1 }
             | '-' float { (- $2) }

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

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = do
  inp <- alexGetInput
  traceShowM inp
  alexError' p ("parse error at token '" ++ show t ++ "'")

parseModule :: FilePath -> String -> Either String Module
parseModule = runAlexWithStartCode layout moduleParser

parseExpression :: FilePath -> String -> Either String Expression
parseExpression = runAlexWithStartCode 0 expressionParser

parseType :: FilePath -> String -> Either String Type
parseType = runAlexWithStartCode 0 typeParser
}
