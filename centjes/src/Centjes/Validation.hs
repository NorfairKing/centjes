{-# LANGUAGE DeriveGeneric #-}

module Centjes.Validation where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Validity (Validity (..))
import GHC.Generics

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
