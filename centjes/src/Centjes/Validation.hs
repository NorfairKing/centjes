{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Validation where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Data.Validity (Validity (..))
import Error.Diagnose
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import System.Exit
import System.IO

-- TODO define Validation in terms of ValidationT so we can use polymorphic functions?
newtype ValidationT e m a = ValidationT {unValidationT :: m (Validation e a)}
  deriving (Functor)

instance (Applicative m) => Applicative (ValidationT e m) where
  pure = ValidationT . pure . Success
  (ValidationT m1) <*> (ValidationT m2) =
    ValidationT $
      (<*>) <$> m1 <*> m2

instance (Monad m) => Monad (ValidationT e m) where
  (ValidationT m) >>= f = ValidationT $ do
    va <- m
    case va of
      Failure es -> pure $ Failure es
      Success a -> unValidationT $ f a

instance MonadTrans (ValidationT e) where
  lift f = ValidationT $ Success <$> f

instance (MonadIO m) => MonadIO (ValidationT e m) where
  liftIO io = ValidationT $ Success <$> liftIO io

instance (MonadLogger m) => MonadLogger (ValidationT e m) where
  monadLoggerLog loc src lvl msg = ValidationT $ do
    monadLoggerLog loc src lvl msg
    pure $ Success ()

runValidationT :: ValidationT e m a -> m (Validation e a)
runValidationT = unValidationT

liftValidation :: (Applicative m) => Validation e a -> ValidationT e m a
liftValidation v = ValidationT $ pure v

validationTFailure :: (Applicative m) => e -> ValidationT e m a
validationTFailure = ValidationT . pure . validationFailure

transformValidationT :: (m (Validation e a) -> n (Validation f b)) -> ValidationT e m a -> ValidationT f n b
transformValidationT func (ValidationT t) = ValidationT $ func t

data Validation e a
  = Failure !(NonEmpty e)
  | Success !a
  deriving (Generic, Show)

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

instance (Semigroup a) => Semigroup (Validation e a) where
  (<>) f1 f2 = (<>) <$> f1 <*> f2

instance (Monoid a) => Monoid (Validation e a) where
  mempty = Success mempty
  mappend = (<>)

validationFailure :: e -> Validation e a
validationFailure e = Failure (e :| [])

mapValidationFailure :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
mapValidationFailure f = \case
  Success a -> Success a
  Failure errs -> Failure $ NE.map f errs

-- Keep this in sync with renderValidationErrors
checkValidation :: (ToReport e) => Diagnostic String -> Validation e a -> IO a
checkValidation diag = \case
  Success a -> pure a
  Failure errs -> dieWithDiag $ renderDiagnostic diag errs

checkValidationPure :: (ToReport e) => Diagnostic String -> Validation e a -> Either Text a
checkValidationPure diag = \case
  Success a -> Right a
  Failure errs -> Left $ renderValidationErrors diag errs

-- Keep this in sync with checkValidation
renderValidationErrors :: (ToReport e) => Diagnostic String -> NonEmpty e -> Text
renderValidationErrors diag errs =
  let diag' :: Diagnostic String
      diag' = renderDiagnostic diag errs
      doc :: Doc AnsiStyle
      doc = defaultStyle <$> prettyDiagnostic WithUnicode (TabSize 2) diag'
   in renderStrict $ layoutPretty defaultLayoutOptions doc

dieWithDiag :: Diagnostic String -> IO a
dieWithDiag diag = do
  printDiagnostic stderr WithUnicode (TabSize 2) defaultStyle diag
  hPutStrLn stderr ""
  hPutStrLn stderr $ unwords [show @Int (length (reportsOf diag)), "errors remaining"]
  exitFailure

renderDiagnostic :: (ToReport e) => Diagnostic String -> NonEmpty e -> Diagnostic String
renderDiagnostic diag errs = foldl' addReport diag (map toReport (NE.toList errs))

class ToReport e where
  toReport :: e -> Report String
