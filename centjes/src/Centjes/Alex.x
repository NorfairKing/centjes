{
{-# OPTIONS -w  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-} -- Because the comments in the generated code said so.
module Centjes.Alex
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  , alexError
  ) where

import Prelude hiding (lex)
import Debug.Trace
import Control.Monad ( liftM )
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text(Text)
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
@emptyline   = @newline @newline+

-- Values
$digit = [0-9]
$alpha = [A-Za-z]

@decimal = $digit+
@integer = 
  = \+ @decimal
  | \- @decimal
  | @decimal


@string = $alpha [$alpha $digit \_ :]*
@year = $digit $digit $digit $digit
@month_of_year = $digit $digit
@day_of_month = $digit $digit
@day = @year \- @month_of_year \- @day_of_month
@comment = "-- " .*

tokens :-

-- Skip non-newline whitespace everywhere
$white_no_nl+ ;

@day                                  { lex TokenDay }
@string                               { lex (TokenString . T.pack) }
@integer                              { lex (TokenInt . read) }
@emptyline                            { lex' TokenEmptyLine }
@newline                              { lex' TokenNewLine }
@comment \n                           { lexComment }

{

-- Count a comment as a newline
lexComment :: AlexInput -> Int -> Alex Token
lexComment inp len = lex (TokenComment . T.pack . drop 3 . init) inp len


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
  = TokenComment Text
  | TokenDay String
  | TokenString Text
  | TokenInt Integer
  | TokenFloat Double
  | TokenEmptyLine
  | TokenNewLine
  | TokenEOF
  deriving ( Show )

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex (TokenComment t) = "-- " <> show t
unLex (TokenDay s) = s
unLex (TokenString t) = show t
unLex (TokenInt i) = show i
unLex (TokenFloat d) = show d
unLex TokenNewLine = "<\\n>"
unLex TokenEmptyLine = "<empty line>"
unLex TokenEOF = "<EOF>"

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
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
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

}
