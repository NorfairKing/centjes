{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Centjes.Validation where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity (Validity (..))
import Error.Diagnose
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import System.Exit

data Validation e a
  = Failure (NonEmpty e)
  | Success a
  deriving (Eq, Generic, Ord, Show)

instance (Validity e, Validity a) => Validity (Validation e a)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Applicative (Validation e) where
  pure = Success
  Failure e1 <*> b = Failure $ case b of
    Failure e2 -> e1 `NE.append` e2
    Success _ -> e1
  Success _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success (f a)

instance Monad (Validation e) where
  return = pure
  Success a >>= f = f a
  Failure es >>= _ = Failure es

validationFailure :: e -> Validation e a
validationFailure e = Failure (e :| [])

validationSuccess :: a -> Validation e a
validationSuccess = Success

mapValidationFailure :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
mapValidationFailure f = \case
  Success a -> Success a
  Failure errs -> Failure $ NE.map f errs

checkValidation :: ToReport e => Diagnostic String -> Validation e a -> IO a
checkValidation diag v =
  case checkValidationPure diag v of
    Right a -> pure a
    Left e ->
      -- TODO make this faster
      -- Maybe we could use something faster than die and unpack
      -- The prettyprinter lib can render directly to stderr
      die $ T.unpack e

checkValidationPure :: ToReport e => Diagnostic String -> Validation e a -> Either Text a
checkValidationPure diag = \case
  Success a -> Right a
  Failure errs -> Left $ renderValidationErrors diag errs

renderValidationErrors :: ToReport e => Diagnostic String -> NonEmpty e -> Text
renderValidationErrors diag errs =
  let diag' :: Diagnostic String
      diag' = foldl' addReport diag (map toReport (NE.toList errs))
      doc :: Doc AnsiStyle
      doc = defaultStyle <$> prettyDiagnostic WithUnicode (TabSize 2) diag'
   in renderStrict $ layoutPretty defaultLayoutOptions doc

class ToReport e where
  toReport :: e -> Report String

exampleReport :: Report String
exampleReport =
  Err
    -- vv  OPTIONAL ERROR CODE
    Nothing
    -- vv  ERROR MESSAGE
    "This is my first error report"
    -- vv  MARKERS
    [(Position (1, 3) (1, 8) "some_test.txt", This "Some text under the marker")]
    -- vv  HINTS
    []
