{
{-# OPTIONS -w  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-} -- Because the comments in the generated code said so.
module Centjes.Alex
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , unLex
  , Alex(..)
  , runAlex
  , runAlex'
  , runAlexInLayout
  , runAlexWithStartCode
  , alexMonadScan'
  , alexError'
  , popLayout
  , alexGetInput
  , lexAll
  , newline
  , layout
  ) where

import Prelude hiding (lex)
import Control.Monad ( liftM )
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
}

%wrapper "monadUserState"

-- GHC's Alex file:
-- https://gitlab.haskell.org/ghc/ghc/-/blob/e2520df3fffa0cf22fb19c5fb872832d11c07d35/compiler/GHC/Parser/Lexer.x

-- Layout-sensitivity based on
-- https://amelia.how/posts/parsing-layout.html

-- Whitespace
$nl          = [\n\r\f]
$whitechar   = [\v\t\ ]
$white_no_nl = $whitechar
$tab         = \t

-- Values
$digit = 0-9
$alpha = [A-Za-z]
$binit = 0-1
$octit = 0-7
$hexit = [$digit A-F a-f]

@numspc = _*  -- Numeric underscores

@negative = \-

@decimal = $digit(@numspc $digit)*
@integer = @decimal

@exponent = @numspc [eE] [\-\+]? @decimal

@floating_point
  = @numspc @decimal \. @decimal @exponent?
  | @numspc @decimal @exponent


@var = $alpha [$alpha $digit \_ \']*
@comment = "-- " .* 

tokens :-

-- Skip non-newline whitespace everywhere
$white_no_nl+ ;


<0> in                                    { lex' TokenIn }
<0> let                                   { layoutKeyword TokenLet }

<0> @var                                  { lex  TokenVar }
<0> @integer                              { lex (TokenInt . read . filter (/= '_')) }
<0> @floating_point                       { lex (TokenFloat . read) }
<0> \->                                   { lex' TokenArrow }
<0> \=                                    { lex' TokenEq }
<0> \-                                    { lex' TokenMinus }
<0> \:                                    { lex' TokenColumn }
<0> \\                                    { lex' TokenBackslash }
<0> \(                                    { lex' TokenLParen }
<0> \)                                    { lex' TokenRParen }

<0> \n                                    { lexNewline } 
<0> @comment \n                           { lexComment }

<layout> {
  \n ; -- Skip empty lines while in the layout context
  () { startLayout }
}

<empty_layout> {
  () { emptyLayout }
}

<newline> {
  \n ; Skip empty lines
  @comment \n { lexComment }
  () { offsideRule }
}

<eof> () { doEOF }
  
{

-- Count a comment as a newline 
lexComment :: AlexInput -> Int -> Alex Token
lexComment inp len = do
  tok <- lex (TokenComment . drop 3 . init) inp len
  pushStartCode newline
  pure tok

-- Slurp up the newline and continue in the newline context
lexNewline :: AlexInput -> Int -> Alex Token
lexNewline inp len = do
  pushStartCode newline 
  alexMonadScan'

-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState
  { alexUserStateSourceFile :: !(Maybe FilePath)
  , alexUserStateStartCodes :: !(NonEmpty Int)
  , alexUserStateLayoutColumns :: ![Int]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 
  { alexUserStateSourceFile = Nothing
  , alexUserStateStartCodes = 0 :| []
  , alexUserStateLayoutColumns = []
  }

-- Helper

alexModifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
alexModifyUserState func = do
  st <- alexGetUserState
  let st' = func st
  alexSetUserState st'

-- Source file

getSourceFilePath :: Alex (Maybe FilePath)
getSourceFilePath = alexUserStateSourceFile <$> alexGetUserState

setSourceFilePath :: FilePath -> Alex ()
setSourceFilePath fp = alexModifyUserState (\s -> s { alexUserStateSourceFile = Just fp})

-- Start codes

getStartCode :: Alex Int
getStartCode = NE.head . alexUserStateStartCodes <$> alexGetUserState

pushStartCode :: Int -> Alex ()
pushStartCode i = do
  alexModifyUserState $ \st ->
    st { alexUserStateStartCodes = NE.cons i (alexUserStateStartCodes st) }
  alexSetStartCode i


-- If there is no start code to go back to, we go back to the 0 start code.
popStartCode :: Alex ()
popStartCode = do
  startCodeStack <- alexUserStateStartCodes <$> alexGetUserState
  let (newCode, newStack) = case startCodeStack of
        _ :| [] -> (0, 0 :| [])
        _ :| (x : xs) -> (x, x :| xs)
  alexModifyUserState $ \st -> st { alexUserStateStartCodes = newStack }
  alexSetStartCode newCode

-- Layout reference columns

getLayout :: Alex (Maybe Int)
getLayout = fmap fst . uncons . alexUserStateLayoutColumns <$> alexGetUserState

pushLayout :: Int -> Alex ()
pushLayout i = alexModifyUserState $ \st ->
  st { alexUserStateLayoutColumns = i : alexUserStateLayoutColumns st }

popLayout :: Alex ()
popLayout = alexModifyUserState $ \st ->
  st { alexUserStateLayoutColumns =
         case alexUserStateLayoutColumns st of
           _ : xs -> xs
           [] -> []
     }

-- Handling layout

layoutKeyword :: TokenClass -> AlexInput -> Int -> Alex Token
layoutKeyword token inp len = do
  pushStartCode layout -- Layout, the start code
  lex' token inp len

startLayout :: AlexInput -> Int -> Alex Token
startLayout inp len = do
  popStartCode
  
  mReferenceColumn <- getLayout
  (AlexPn _ _ currentColumn,_,_,_) <- alexGetInput

  if Just currentColumn <= mReferenceColumn
    then pushStartCode empty_layout
    else pushLayout currentColumn
  
  lex' TokenLayoutOpen inp len

emptyLayout :: AlexInput -> Int -> Alex Token
emptyLayout inp len = do
  popStartCode
  pushStartCode newline
  lex' TokenLayoutClose inp len

offsideRule :: AlexInput -> Int -> Alex Token
offsideRule inp len = do
  mReferenceColumn <- getLayout
  (AlexPn _ _ currentColumn,_,_,_) <- alexGetInput

  let continue = popStartCode *> alexMonadScan'

  case mReferenceColumn of
    Just referenceColumn -> do
      case currentColumn `compare` referenceColumn of
        EQ -> do
          popStartCode
          lex' TokenLayoutDelimit inp len
        GT -> continue
        LT -> do
          popLayout
          lex' TokenLayoutClose inp len
    _ -> continue

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving ( Show )

data TokenClass
  =
  -- Keywords
    TokenLet
  | TokenIn

  -- Comment
  | TokenComment String

  -- Identifier
  | TokenVar String

  -- Literals
  | TokenInt Integer
  | TokenFloat Double

  -- Symbols
  | TokenArrow
  | TokenEq
  | TokenMinus
  | TokenColumn
  | TokenBackslash
  | TokenLParen
  | TokenRParen

  -- Layout tokens
  | TokenLayoutOpen
  | TokenLayoutDelimit
  | TokenLayoutClose

  -- End of file
  | TokenEOF
  deriving ( Show )

-- For nice parser error messages.
unLex :: TokenClass -> String
unLex TokenLet = "let"
unLex TokenIn = "in"
unLex (TokenComment s) = "-- " <> s
unLex (TokenVar s) = show s
unLex (TokenInt i) = show i
unLex (TokenFloat d) = show d
unLex TokenArrow = "->"
unLex TokenEq = "="
unLex TokenMinus = "-"
unLex TokenColumn = ":"
unLex TokenBackslash = "\\"
unLex TokenLParen = "("
unLex TokenRParen = ")"
unLex TokenLayoutOpen = "{"
unLex TokenLayoutDelimit = "{"
unLex TokenLayoutClose = "}"
unLex TokenEOF = "<EOF>"

-- Lexing tokens

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const


-- Dealing with EOF

alexEOF :: Alex Token
alexEOF = do
  pushStartCode eof *> alexMonadScan'

-- Close all layout contexts
doEOF :: AlexInput -> Int -> Alex Token
doEOF inp len = do
  mReferenceColumn <- getLayout
  case mReferenceColumn of
    Nothing -> do
      -- Not in any layout, just the end
      popStartCode
      lex' TokenEOF inp len
    Just referenceColumn -> do
      -- In a layout, end it
      popLayout
      lex' TokenLayoutClose inp len

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
runAlex' = runAlexInLayout

runAlexInLayout = runAlexWithStartCode layout

runAlexWithStartCode :: Int -> Alex a -> FilePath -> String -> Either String a
runAlexWithStartCode startCode func fp input = 
  runAlex input $ do
    setSourceFilePath fp
    pushStartCode startCode
    func

lexAll :: Alex ()
lexAll = do
  token <- alexMonadScan'
  case token of
    Token _ TokenEOF -> pure ()
    _ -> lexAll
}
