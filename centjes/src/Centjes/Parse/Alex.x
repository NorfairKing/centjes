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
  , getSourceFilePath
  , getsAlex
  ) where

import Centjes.Location
import Control.Monad (liftM)
import Data.List
import Data.Maybe (fromJust)
import Data.Text (Text)
import Numeric.DecimalLiteral
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

@unsigned_scientific =
    @decimal
  | @decimal \. @decimal

@scientific =
    \+? @unsigned_scientific
  | \- @unsigned_scientific

@var = $alpha [$alpha $digit \_ \- :]*
@year = $digit $digit $digit $digit
@month_of_year = $digit $digit
@day_of_month = $digit $digit
@day = @year \- @month_of_year \- @day_of_month

@path = [$alpha $digit \_ \- : .]+

@star = \*
@dot = \.
@import = "import " .* \n
@currency = "currency "
@account = "account "
@attach = "+ attach " @path

@comment = "-- " .* \n

@description = "| " .* \n

tokens :-

-- Skip non-newline whitespace everywhere
$white_no_nl+ ;

@import             { lex (TokenImport . drop (length "import ") . init) }
@day                { lex TokenDay }
@attach             { lex (TokenAttachment . drop (length "+ attach ")) }
@comment            { lex (TokenComment . T.pack . drop (length "-- ") . init) }
@scientific         { lex (TokenDecimalLiteral . fromJust . parseDecimalLiteral) } -- TODO get rid of fromJust
@dot                { lex' TokenDot}
@star               { lex' TokenStar}
@currency           { lex' TokenCurrency}
@account            { lex' TokenAccount}
@description        { lex (TokenDescription . T.pack . drop (length "| ") . init) }
@newline            { lex' TokenNewLine }
@var                { lex (TokenVar . T.pack) }

{

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState
  { sourceFilePath :: FilePath
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState ""

getSourceFilePath :: Alex FilePath
getSourceFilePath = liftM sourceFilePath alexGetUserState

setSourceFilePath :: FilePath -> Alex ()
setSourceFilePath = alexSetUserState . AlexUserState


-- The token type, consisting of the source code position and a token class.
type Token = GenLocated SourceSpan TokenClass

data TokenClass
  = TokenComment !Text
  | TokenAttachment !FilePath
  | TokenDay !String
  | TokenVar !Text
  | TokenDescription !Text
  | TokenDecimalLiteral !DecimalLiteral
  | TokenFloat !Double
  | TokenStar
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
  path <- getSourceFilePath
  pure SourceSpan
        { sourceSpanFile = path
        , sourceSpanBegin =alexSourcePosition beginPos
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
lex f = \(p,_,_,s) i -> do
  let begin = alexSourcePosition p
  let end = begin { sourcePositionColumn = sourcePositionColumn begin + i}
  path <- getSourceFilePath
  let span = SourceSpan
        { sourceSpanFile = path
        , sourceSpanBegin = begin
        , sourceSpanEnd = end
        }
  return $ Located span (f (take i s))

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
alexError' (SourceSpan _ begin _) msg = do
  let l = sourcePositionLine begin
  let c = sourcePositionColumn begin
  fp <- getSourceFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setSourceFilePath fp >> a)

scanMany :: String -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      token@(Located _ c) <- alexMonadScan'
      if c == TokenEOF
        then pure [token]
        else (token :) <$> go

getsAlex :: (AlexState -> a) -> Alex a
getsAlex func = Alex (\as -> Right (as, func as))
}

