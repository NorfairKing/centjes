{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Centjes.Validation where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Validity (Validity (..))
import Error.Diagnose
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import System.Exit

newtype ValidationT e m a = ValidationT {unValidationT :: m (Validation e a)}
  deriving (Functor)

instance Applicative m => Applicative (ValidationT e m) where
  pure = ValidationT . pure . Success
  (ValidationT m1) <*> (ValidationT m2) =
    ValidationT $
      (<*>) <$> m1 <*> m2

instance Monad m => Monad (ValidationT e m) where
  (ValidationT m) >>= f = ValidationT $ do
    va <- m
    case va of
      Failure es -> pure $ Failure es
      Success a -> unValidationT $ f a

instance MonadTrans (ValidationT e) where
  lift f = ValidationT $ Success <$> f

instance MonadIO m => MonadIO (ValidationT e m) where
  liftIO io = ValidationT $ Success <$> liftIO io

runValidationT :: ValidationT e m a -> m (Validation e a)
runValidationT = unValidationT

liftValidation :: Applicative m => Validation e a -> ValidationT e m a
liftValidation v = ValidationT $ pure v

validationTFailure :: Applicative m => e -> ValidationT e m a
validationTFailure = ValidationT . pure . validationFailure

mapValidationTFailure :: Functor m => (e1 -> e2) -> ValidationT e1 m a -> ValidationT e2 m a
mapValidationTFailure f (ValidationT m) = ValidationT $ mapValidationFailure f <$> m

transformValidationT :: (m (Validation e a) -> n (Validation f b)) -> ValidationT e m a -> ValidationT f n b
transformValidationT func (ValidationT t) = ValidationT $ func t

data Validation e a
  = Failure !(NonEmpty e)
  | Success !a
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

-- Keep this in sync with renderValidationErrors
checkValidation :: ToReport e => Diagnostic String -> Validation e a -> IO a
checkValidation diag = \case
  Success a -> pure a
  Failure errs -> dieWithDiag $ renderDiagnostic diag errs

checkValidationPure :: ToReport e => Diagnostic String -> Validation e a -> Either Text a
checkValidationPure diag = \case
  Success a -> Right a
  Failure errs -> Left $ renderValidationErrors diag errs

-- Keep this in sync with checkValidation
renderValidationErrors :: ToReport e => Diagnostic String -> NonEmpty e -> Text
renderValidationErrors diag errs =
  let diag' :: Diagnostic String
      diag' = renderDiagnostic diag errs
      doc :: Doc AnsiStyle
      doc = defaultStyle <$> prettyDiagnostic WithUnicode (TabSize 2) diag'
   in renderStrict $ layoutPretty defaultLayoutOptions doc

dieWithDiag :: Diagnostic String -> IO a
dieWithDiag diag = do
  printDiagnostic stderr WithUnicode (TabSize 2) defaultStyle diag
  exitFailure

renderDiagnostic :: ToReport e => Diagnostic String -> NonEmpty e -> Diagnostic String
renderDiagnostic diag errs = foldl' addReport diag (map toReport (NE.toList errs))

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
