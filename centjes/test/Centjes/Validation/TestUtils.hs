{-# LANGUAGE LambdaCase #-}

module Centjes.Validation.TestUtils
  ( shouldValidate,
  )
where

import Centjes.Validation
import Control.Exception
import qualified Data.List.NonEmpty as NE
import Test.Syd

shouldValidate :: Exception e => Validation e a -> IO a
shouldValidate = \case
  Failure errs ->
    expectationFailure $
      unlines ("Failed:" : map displayException (NE.toList errs))
  Success a -> pure a
