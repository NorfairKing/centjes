{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Description
  ( Description (..),
    nullDescription,
    validateDescriptionChar,
    fromTextM,
    fromText,
  )
where

import Control.DeepSeq
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Description where
  validate an@(Description t) =
    mconcat
      [ genericValidate an,
        decorateText t validateDescriptionChar
      ]

validateDescriptionChar :: Char -> Validation
validateDescriptionChar = \c -> declare "The character is not a newline, and not a control character" $
  case c of
    '\n' -> False
    '\r' -> False
    _
      | Char.isControl c -> False
      | otherwise -> True

instance NFData Description

nullDescription :: Description -> Bool
nullDescription = T.null . unDescription

fromTextM :: MonadFail m => Text -> m Description
fromTextM t = case fromText t of
  Nothing -> fail $ "Invalid description: " <> show t
  Just d -> pure d

fromText :: Text -> Maybe Description
fromText = constructValid . Description
