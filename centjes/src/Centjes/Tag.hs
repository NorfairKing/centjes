{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Tag
  ( Tag (..),
    Centjes.Tag.fromString,
    fromText,
    toText,
    toString,
  )
where

import Autodocodec
import qualified Data.Char as Char
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Tag = Tag {tagText :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show)

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

instance HasCodec Tag where
  codec = bimapCodec Centjes.Tag.fromText toText codec

instance IsString Tag where
  fromString s = case Centjes.Tag.fromString s of
    Left err -> error err
    Right t -> t

fromString :: String -> Either String Tag
fromString = fromText . T.pack

fromText :: Text -> Either String Tag
fromText = prettyValidate . Tag

toText :: Tag -> Text
toText = tagText

toString :: Tag -> String
toString = T.unpack . toText
