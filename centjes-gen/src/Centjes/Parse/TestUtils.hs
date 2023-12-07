{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Parse.TestUtils where

import Centjes.Parse.Alex
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Test.Syd

shouldParse :: HasCallStack => (filepath -> Text -> Either String a) -> filepath -> Text -> IO a
shouldParse parser fp contents = withFrozenCallStack $
  case parser fp contents of
    Left err -> do
      expectationFailure $
        unlines $
          concat
            [ [ "Failed to parse:",
                err
              ],
              [ "",
                "tokens:",
                ppShow (scanMany (T.unpack contents))
              ]
            ]
    Right m -> pure m
