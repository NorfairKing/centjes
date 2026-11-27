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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Validity (Validity (..))
import qualified Data.Validity as Validity
import Error.Diagnose
import GHC.Generics
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, renderStrict)
import System.Exit
import System.IO

{-# ANN module ("DisableMutations" :: String) #-}

-- TODO define Validation in terms of ValidationT so we can use polymorphic functions?
-- [allow:CommentBareTodo] Open work without an issue yet.
newtype ValidationT e m a = ValidationT {unValidationT :: m (Validation e a)}
  deriving (Functor)

instance (Applicative m) => Applicative (ValidationT e m) where
  pure = ValidationT . pure . ValidationSuccess
  (ValidationT m1) <*> (ValidationT m2) =
    ValidationT $
      (<*>) <$> m1 <*> m2

instance (Monad m) => Monad (ValidationT e m) where
  (ValidationT m) >>= f = ValidationT $ do
    va <- m
    case va of
      ValidationFailure es -> pure $ ValidationFailure es
      ValidationSuccess a -> unValidationT $ f a

instance MonadTrans (ValidationT e) where
  lift f = ValidationT $ ValidationSuccess <$> f

instance (MonadIO m) => MonadIO (ValidationT e m) where
  liftIO io = ValidationT $ ValidationSuccess <$> liftIO io

instance (MonadLogger m) => MonadLogger (ValidationT e m) where
  monadLoggerLog loc src lvl msg = ValidationT $ do
    monadLoggerLog loc src lvl msg
    pure $ ValidationSuccess ()

runValidationT :: ValidationT e m a -> m (Validation e a)
runValidationT = unValidationT

liftValidation :: (Applicative m) => Validation e a -> ValidationT e m a
liftValidation v = ValidationT $ pure v

validationTFailure :: (Applicative m) => e -> ValidationT e m a
validationTFailure = ValidationT . pure . validationFailure

data Validation e a
  = ValidationFailure !(NonEmpty e)
  | ValidationSuccess !a
  deriving (Generic, Show)

instance (Validity e, Validity a) => Validity (Validation e a)

instance Functor (Validation e) where
  fmap _ (ValidationFailure e) = ValidationFailure e
  fmap f (ValidationSuccess a) = ValidationSuccess (f a)

instance Applicative (Validation e) where
  pure = ValidationSuccess
  ValidationFailure e1 <*> ValidationFailure e2 = ValidationFailure (e1 `NE.append` e2)
  ValidationFailure e1 <*> ValidationSuccess _ = ValidationFailure e1
  ValidationSuccess _ <*> ValidationFailure e2 = ValidationFailure e2
  ValidationSuccess f <*> ValidationSuccess a = ValidationSuccess (f a)

instance Monad (Validation e) where
  return = pure
  ValidationSuccess a >>= f = f a
  ValidationFailure es >>= _ = ValidationFailure es

instance (Semigroup a) => Semigroup (Validation e a) where
  (<>) f1 f2 = (<>) <$> f1 <*> f2

instance (Monoid a) => Monoid (Validation e a) where
  mempty = ValidationSuccess mempty
  mappend = (<>)

validationFailure :: e -> Validation e a
validationFailure e = ValidationFailure (e :| [])

mapValidationFailure :: (e1 -> e2) -> Validation e1 a -> Validation e2 a
mapValidationFailure f = \case
  ValidationSuccess a -> ValidationSuccess a
  ValidationFailure errs -> ValidationFailure $ NE.map f errs

-- Keep this in sync with renderValidationErrors
checkValidation :: (ToReport e) => Diagnostic String -> Validation e a -> IO a
checkValidation diag = \case
  ValidationSuccess a -> pure a
  ValidationFailure errs -> dieWithDiag $ renderDiagnostic diag errs

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

-- | Why a value is not valid, as Validity explains it, for a library that
-- wants a failure as a String.
renderInvalid :: [Validity.ValidationChain] -> String
renderInvalid = fromMaybe "" . Validity.prettyValidation . Validity.Validation
