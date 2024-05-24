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
  , eitherParser
  ) where

import Centjes.Location
import Data.Text (Text)
import Numeric.DecimalLiteral as DecimalLiteral
import Path
import Prelude hiding (lex)
import qualified Data.Text as T
}

%wrapper "monadUserState"

-- GHC's Alex file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser/Lexer.x

-- Whitespace
$nl          = [\n\r\f]
$tab         = \t
@newline     = $nl
-- TODO remove this and use $white
@white       = [\n\r\f\v\t\ ]

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

@var = $alpha [$alpha $digit \_ \- \:]*
@year = $digit $digit $digit $digit
@month_of_year = $digit $digit
@day_of_month = $digit $digit
@day = @year \- @month_of_year \- @day_of_month

@hour = $digit $digit
@minute = $digit $digit
@second = $digit $digit
@time_of_day =
    @hour \: @minute \: @second
  | @hour \: @minute

@timestamp =
    @day
  | @day \  @time_of_day

@file_path = [$alpha $digit \_ \- \: .]+
@anyline = [^\n\r]+

@pipe = "| "
@star = "* "
@bang = "! "
@plus = "+ " 
@at = "@ "
@slash = "/ "
@percent = "%"
@tilde = "~"
@import = "import "
@currency = "currency "
@account = "account "
@attach = "attach "
@assert = "assert "
@tag = "tag "
@price = "price "
@eq = \=
@doubledash = "-- "

tokens :-

-- The 0 start code means "toplevel declaration"

-- Imports
-- Note: 'import' is not a valid haskell identifier so we use imp instead.
<0> @import             { lex' TokenImport `andBegin` imp }
<imp> @file_path        { lex TokenFilePath `andBegin` 0 }

-- Comments
<0> @doubledash            { lex' TokenDoubleDash `andBegin` comment }
<comment> @anyline         { lex (TokenAnyLine . T.pack) }
<comment> @newline@white*         { begin 0 }

-- Currency declarations
<0> @currency               { lex' TokenCurrency `andBegin` currency }
<currency> @var             { lexVar }
<currency> @decimal_literal { lexDL `andBegin` 0 }

-- Account declarations
<0> @account        { lex' TokenAccount `andBegin` account }
<account> @var     { lexVar }
<account> @newline@white* { begin 0 }

-- Tag declarations
<0> @tag        { lex' TokenTag `andBegin` dec_tag }
<dec_tag> @var     { lexVar `andBegin` 0 }

-- Price declarations
<0> @price               { lex' TokenPrice `andBegin` price }
<price> @timestamp       { lexTimestamp }
<price> @var             { lexVar }
<price> @decimal_literal { lexDL }
<price> @slash           { lexSlash }
<price> @newline@white*         { begin 0 }

-- Transactions
<0> @timestamp { lexTimestamp }

<0> @pipe              { lex' TokenPipe `andBegin` description }
<description> @anyline { lex (TokenAnyLine . T.pack) }
<description> @newline@white* { begin 0 }

<0> @star        { lex' TokenStar `andBegin` posting }
<0> @bang        { lex' TokenBang `andBegin` posting }
<posting> @var             { lexVar }
<posting> @decimal_literal { lexDL }
<posting> @at              { lexAt }
<posting> @slash           { lexSlash }
<posting> @tilde           { lexTilde }
<posting> @percent         { lexPercent }
<posting> @newline@white*  { begin 0 }


<0> @plus  { lex' TokenPlus `andBegin` extra }

<extra> @assert             { lex' TokenAssert `andBegin` assertion }
<assertion> @var             { lexVar }
<assertion> @eq              { lex' TokenEq }
<assertion> @decimal_literal { lexDL }
<assertion> @newline@white*  { begin 0 }

<extra> @attach         { lex' TokenAttach `andBegin` attachment }
<attachment> @file_path { lex TokenFilePath }
<attachment> @newline   { begin 0 }
<attachment> @newline@white*  { begin 0 }

<extra> @tag            { lex' TokenTag `andBegin` tag }
<tag> @var       { lexVar }
<tag> @newline@white*  { begin 0 }

-- Skip any other whitespace everywhere
@white+ ;
{
lexTimestamp :: AlexAction Token
lexTimestamp = lex TokenTimestamp 

lexVar :: AlexAction Token
lexVar = lex (TokenVar . T.pack)

lexDL :: AlexAction Token
lexDL = lexM (maybeParser "DecimalLiteral" (fmap TokenDecimalLiteral . DecimalLiteral.fromString))

lexAt :: AlexAction Token
lexAt = lex' TokenAt

lexSlash :: AlexAction Token
lexSlash = lex' TokenSlash

lexTilde :: AlexAction Token
lexTilde = lex' TokenTilde

lexPercent :: AlexAction Token
lexPercent = lex' TokenPercent

lexNl :: AlexAction Token
lexNl = lex' TokenNewLine

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState
  { sourceFileBase :: Path Abs Dir
  , sourceFilePath :: Path Rel File
  }

alexInitUserState :: AlexUserState
alexInitUserState =
  -- Laziness saves our butt
  AlexUserState undefined undefined


-- The token type, consisting of the source code position and a token class.
type Token = GenLocated SourceSpan TokenClass

data TokenClass
  = TokenAttach
  | TokenAssert
  | TokenTag
  | TokenPrice
  | TokenEq
  | TokenTimestamp !String
  | TokenFilePath !FilePath
  | TokenVar !Text
  | TokenDecimalLiteral !DecimalLiteral
  | TokenFloat !Double
  | TokenDoubleDash
  | TokenPipe
  | TokenAnyLine !Text
  | TokenStar
  | TokenBang
  | TokenPlus
  | TokenDot
  | TokenAt
  | TokenSlash
  | TokenTilde
  | TokenPercent
  | TokenCurrency
  | TokenAccount
  | TokenImport
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
  let end = begin { sourcePositionColumn = sourcePositionColumn begin + i }
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
              (' ': _) -> "<space>"
              ('\t': _) -> "<tab>"
              ('\n': _) -> "<newline>"
              (c:_) -> show c
        span <- spanFromSingle p
        state <- alexGetStartCode
        alexError' span ("lexical error at character " ++ desc ++ " currently in state " ++ show state)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: SourceSpan -> String -> Alex a
alexError' (SourceSpan _ _ begin end) msg = do
  state <- alexGetUserState
  let showPos pos = show (sourcePositionLine pos) ++ ":" ++ show (sourcePositionColumn pos)
  alexError (fromRelFile (sourceFilePath state) ++ "@" ++ showPos begin ++ "-" ++ showPos end ++ "  " ++ msg)

maybeParser :: Show b => String -> (b -> Maybe a) -> b -> Alex a
maybeParser name func b =
  case func b of
    Nothing -> alexError $ "Failed to parse " <> name <> " from " <> show b
    Just a -> pure a

eitherParser :: Show b => String -> (b -> Either String a) -> b -> Alex a
eitherParser name func b =
  case func b of
    Left err -> alexError $ "Failed to parse " <> name <> " from " <> show b <> ":\n" <> err
    Right a -> pure a

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

