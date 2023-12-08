{
{-# OPTIONS -w -Wunused-imports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-} -- Because the comments in the generated code said so.
module Centjes.Parse.Alex
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , AlexState(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , alexError
  , scanMany
  , getsAlex
  , maybeParser
  ) where

import Centjes.Location
import Data.Text (Text)
import Numeric.DecimalLiteral
import Path
import Prelude hiding (lex)
import qualified Data.Text as T
}

%wrapper "monadUserState"

-- GHC's Alex file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser/Lexer.x

-- Whitespace
$nl          = [\n\r\f]
$whitechar   = [\v\t\ ]
$white_no_nl = $whitechar
$tab         = \t
@newline     = $nl

-- Values
$digit = [0-9]
$alpha = [A-Za-z]

@decimal = $digit+

@natural = @decimal

@unsigned_decimal_literal =
    @decimal
  | @decimal \. @decimal

@decimal_literal =
    \+? @unsigned_decimal_literal
  | \- @unsigned_decimal_literal

@var = $alpha [$alpha $digit \_ \- :]*
@year = $digit $digit $digit $digit
@month_of_year = $digit $digit
@day_of_month = $digit $digit
@day = @year \- @month_of_year \- @day_of_month

@file_path = [$alpha $digit \_ \- : .]+

@star = "* "
@plus = "+ " 
@import = "import " .* \n
@currency = "currency "
@account = "account "
@attach = "attach "
@assert = "assert "
@eq = \=

@comment = "-- " .* \n

@description = "| " .* \n

tokens :-

-- Skip non-newline whitespace everywhere
$white_no_nl+ ;

-- The 0 start code means "toplevel declaration"

<0> @newline            { lexNl }

-- Imports
<0> @import             { lex (TokenImport . drop (length "import ") . init) }

-- Comments
<0> @comment            { lex (TokenComment . T.pack . drop (length "-- ") . init) }

-- Currency declarations
<0> @currency               { lex' TokenCurrency `andBegin` currency }
<currency> @var             { lexVar }
<currency> @decimal_literal { lexDL }
<currency> @newline         { lexNl `andBegin` 0 }

-- Account declarations
<0> @account        { lex' TokenAccount `andBegin` account}
<account> @var     { lexVar }
<account> @newline { lexNl `andBegin` 0 }


-- Transactions
<0> @day                { lex TokenDay `andBegin` transaction_header }

-- We need a separate state for the newline after the day
<transaction_header>  @newline  { lexNl `andBegin` transaction}

<transaction> @description  { lex (TokenDescription . T.pack . drop (length "| ") . init) }
<transaction> @newline      { lexNl `andBegin` 0}


<transaction> @star                  { lex' TokenStar `andBegin` posting }
<posting> @var             { lexVar }
<posting> @decimal_literal { lexDL }
<posting> @newline         { lexNl `andBegin` transaction}


<transaction> @plus  { lex' TokenPlus `andBegin` extra }

<extra> @assert                  { lex' TokenAssert `andBegin` assertion }
<assertion> @var             { lexVar }
<assertion> @eq              { lex' TokenEq }
<assertion> @decimal_literal { lexDL }
<assertion> @newline         { lexNl `andBegin` transaction}

<extra> @attach             { lex' TokenAttach `andBegin` attachment}
<attachment> @file_path { lex TokenFilePath }
<attachment> @newline   { lexNl `andBegin` transaction}

{

lexVar :: AlexAction Token
lexVar = lex (TokenVar . T.pack)

lexDL :: AlexAction Token
lexDL = lexM (maybeParser "DecimalLiteral" (fmap TokenDecimalLiteral . parseDecimalLiteral))

lexNl :: AlexAction Token
lexNl = lex' TokenNewLine

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState
  { sourceFileBase :: Path Abs Dir
  , sourceFilePath :: Path Rel File
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState undefined undefined -- Laziness saves our butt


-- The token type, consisting of the source code position and a token class.
type Token = GenLocated SourceSpan TokenClass

data TokenClass
  = TokenComment !Text
  | TokenAttach
  | TokenAssert
  | TokenEq
  | TokenDay !String
  | TokenFilePath !FilePath
  | TokenVar !Text
  | TokenDescription !Text
  | TokenDecimalLiteral !DecimalLiteral
  | TokenFloat !Double
  | TokenStar
  | TokenPlus
  | TokenDot
  | TokenCurrency
  | TokenAccount
  | TokenImport !FilePath
  | TokenNewLine
  | TokenEOF
  deriving ( Show, Eq )

spanFromSingle :: AlexPosn -> Alex SourceSpan
spanFromSingle posn = spanFromBeginEnd posn posn

spanFromBeginEnd :: AlexPosn -> AlexPosn -> Alex SourceSpan
spanFromBeginEnd beginPos endPos = do
  state <- alexGetUserState
  pure SourceSpan
        { sourceSpanBase = sourceFileBase state
        , sourceSpanFile = sourceFilePath state
        , sourceSpanBegin = alexSourcePosition beginPos
        , sourceSpanEnd = alexSourcePosition endPos
        }

alexSourcePosition :: AlexPosn -> SourcePosition
alexSourcePosition (AlexPn _ l c) =
  SourcePosition
    { sourcePositionLine = l
    , sourcePositionColumn = c
    }

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  s <- spanFromSingle p
  return $ Located s TokenEOF


-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = lexM (pure . f)

lexM :: (String -> Alex TokenClass) -> AlexAction Token
lexM f = \(p,_,_,s) i -> do
  let begin = alexSourcePosition p
  let end = begin { sourcePositionColumn = sourcePositionColumn begin + i}
  state <- alexGetUserState
  let span =
        SourceSpan
          { sourceSpanBase = sourceFileBase state
          , sourceSpanFile = sourceFilePath state
          , sourceSpanBegin = begin
          , sourceSpanEnd = end
          }
  tc <- f (take i s)
  return $ Located span tc

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) -> do
        let desc = case s of
              [] -> "<empty>"
              (c:_) -> show c
        span <- spanFromSingle p
        alexError' span ("lexical error at character '" ++ desc ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: SourceSpan -> String -> Alex a
alexError' (SourceSpan _ _ begin _) msg = do
  state <- alexGetUserState
  let l = sourcePositionLine begin
  let c = sourcePositionColumn begin
  alexError (fromRelFile (sourceFilePath state) ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

maybeParser :: Show b => String -> (b -> Maybe a) -> b -> Alex a
maybeParser name func b =
  case func b of
    Nothing -> alexError $ "Failed to parse " <> name <> " from " <> show b
    Just a -> pure a


runAlex' :: Alex a -> Path Abs Dir -> Path Rel File -> String -> Either String a
runAlex' a base fp input = runAlex input (alexSetUserState (AlexUserState base fp) >> a)

scanMany :: Path Abs Dir -> Path Rel File -> String -> Either String [Token]
scanMany base fp input = runAlex' go base fp input 
  where
    go = do
      token@(Located _ c) <- alexMonadScan'
      if c == TokenEOF
        then pure [token]
        else (token :) <$> go

getsAlex :: (AlexState -> a) -> Alex a
getsAlex func = Alex (\as -> Right (as, func as))
}

