{-# LANGUAGE LambdaCase #-}

module Centjes.Validation.TestUtils
  ( shouldValidate,
    shouldFailToValidateT,
    shouldFailToValidate,
  )
where

import Centjes.Validation
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Error.Diagnose
import Test.Syd

shouldValidate :: (ToReport e) => Diagnostic String -> Validation e a -> IO a
shouldValidate diag v = case checkValidationPure diag v of
  Left e -> expectationFailure $ T.unpack e
  Right a -> pure a

shouldFailToValidateT :: (Show a) => ValidationT e IO a -> IO (NonEmpty e)
shouldFailToValidateT (ValidationT f) = f >>= shouldFailToValidate

shouldFailToValidate :: (Show a) => Validation e a -> IO (NonEmpty e)
shouldFailToValidate = \case
  Failure errs -> pure errs
  Success a ->
    expectationFailure $
      unlines
        [ "Should have failed, but resulted in",
          ppShow a
        ]
