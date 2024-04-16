{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Tag
  ( Tag (..),
    fromText,
    toText,
    toString,
  )
where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Tag = Tag {tagText :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Tag where
  validate an@(Tag t) =
    mconcat
      [ genericValidate an,
        declare "The currency symbol is not empty" $ not (T.null t),
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character, or _, or -, or :" $
          case c of
            ':' -> True
            '-' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

fromText :: Text -> Either String Tag
fromText = prettyValidate . Tag

toText :: Tag -> Text
toText = tagText

toString :: Tag -> String
toString = T.unpack . toText
