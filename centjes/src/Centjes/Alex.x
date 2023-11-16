{
{-# OPTIONS -w  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-} -- Because the comments in the generated code said so.
module Centjes.Alex
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , alexError
  , scanMany
  ) where

import Centjes.DecimalLiteral
import Control.Monad ( liftM )
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Ratio
import Data.Text(Text)
import Debug.Trace
import Numeric (readFloat, readSigned)
import Numeric.Natural
import Prelude hiding (lex)
import Text.ParserCombinators.ReadP
import qualified Data.List.NonEmpty as NE
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

@var = $alpha [$alpha $digit \_ :]*
@year = $digit $digit $digit $digit
@month_of_year = $digit $digit
@day_of_month = $digit $digit
@day = @year \- @month_of_year \- @day_of_month

@star = \*
@dot = \.
@import = "import " .* \n
@currency = "currency "

@comment = "-- " .* \n

@description = "| " .* \n

tokens :-

-- Skip non-newline whitespace everywhere
$white_no_nl+ ;

@import             { lex (TokenImport . drop (length "import ") . init) }
@day                { lex TokenDay }
@comment            { lexComment }
@scientific         { lex (TokenDecimalLiteral . fromJust . parseDecimalLiteral) } -- TODO get rid of fromJust
@dot                { lex' TokenDot}
@star               { lex' TokenStar}
@currency           { lex' TokenCurrency}
@description        { lex (TokenDescription . T.pack . drop (length "| ") . init) }
@newline            { lex' TokenNewLine }
@var                { lex (TokenVar . T.pack) }

{

-- Count a comment as a newline
lexComment :: AlexInput -> Int -> Alex Token
lexComment inp len = lex (TokenComment . T.pack . drop (length "-- ") . init) inp len


-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState
  { sourceFilePath :: Maybe FilePath
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState Nothing

getSourceFilePath :: Alex (Maybe FilePath)
getSourceFilePath = liftM sourceFilePath alexGetUserState

setSourceFilePath :: FilePath -> Alex ()
setSourceFilePath = alexSetUserState . AlexUserState . Just


-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  = TokenComment !Text
  | TokenDay !String
  | TokenVar !Text
  | TokenDescription !Text
  | TokenDecimalLiteral !DecimalLiteral
  | TokenFloat !Double
  | TokenStar
  | TokenDot
  | TokenCurrency
  | TokenImport !FilePath
  | TokenNewLine
  | TokenEOF
  deriving ( Show, Eq )

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF


-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

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
        alexError' p ("lexical error at character '" ++ desc ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  mfp <- getSourceFilePath
  alexError (maybe "" (++ ":") mfp ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setSourceFilePath fp >> a)


scanMany :: String -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      token@(Token _ c) <- alexMonadScan'
      if c == TokenEOF
        then pure [token]
        else (token :) <$> go
}
