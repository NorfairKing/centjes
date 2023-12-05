{-# LANGUAGE LambdaCase #-}

module Centjes.Validation.TestUtils
  ( shouldValidate,
    shouldFailToValidateT,
    shouldFailToValidate,
  )
where

import Centjes.Validation
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Test.Syd

shouldValidate :: Show e => Validation e a -> IO a
shouldValidate = \case
  Success a -> pure a
  Failure errs ->
    expectationFailure $
      unlines ("Failed:" : map show (NE.toList errs))

shouldFailToValidateT :: Show a => ValidationT e IO a -> IO (NonEmpty e)
shouldFailToValidateT (ValidationT f) = f >>= shouldFailToValidate

shouldFailToValidate :: Show a => Validation e a -> IO (NonEmpty e)
shouldFailToValidate = \case
  Failure errs -> pure errs
  Success a ->
    expectationFailure $
      unlines
        [ "Should have failed, but resulted in",
          ppShow a
        ]
