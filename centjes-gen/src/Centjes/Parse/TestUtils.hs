{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Parse.TestUtils where

import Centjes.Parse.Alex
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Path
import Test.Syd

shouldParse ::
  HasCallStack =>
  (Path Abs Dir -> Path Rel File -> Text -> Either String a) ->
  Path Abs Dir ->
  Path Rel File ->
  Text ->
  IO a
shouldParse parser base fp contents = withFrozenCallStack $
  case parser base fp contents of
    Left err -> do
      expectationFailure $
        unlines $
          concat
            [ [ "Failed to parse:",
                err
              ],
              [ "",
                "tokens:",
                ppShow (scanMany base fp (T.unpack contents))
              ]
            ]
    Right m -> pure m
