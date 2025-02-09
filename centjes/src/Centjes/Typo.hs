{-# LANGUAGE LambdaCase #-}

module Centjes.Typo (isTypoOf) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Test whether a text could be a typo of another
--
-- Note that any text is considered a typo of itself as well.
isTypoOf :: Text -> Text -> Bool
isTypoOf t1 t2
  | t1 == t2 = True
  -- Can only be a typo if the lengths are less than 2 different.
  | abs (T.length t1 - T.length t2) >= 2 = False
  | otherwise = go (T.unpack t1) (T.unpack t2)
  where
    -- Equal is fine for go, because it is checked earlier.
    go :: [Char] -> [Char] -> Bool
    go [] [] = True
    go [_] [] = True -- insertion
    go (_ : _ : _) [] = False
    go [] [_] = True -- Deletion
    go [] (_ : _ : _) = False
    go s1@(c1 : cs1) s2@(c2 : cs2)
      | c1 == c2 = go cs1 cs2
      -- Inserted c2
      | s1 == cs2 = True
      -- Deleted c1
      | cs1 == s2 = True
      -- Mis-typed c1 as c2
      | otherwise = cs1 == cs2
