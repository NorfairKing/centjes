{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Tag
  ( Tag (..),
    Centjes.Tag.fromString,
    fromText,
    toText,
    toString,
    isTypoOf,
  )
where

import Autodocodec
import qualified Centjes.Typo as Text
import Centjes.Validation (renderInvalid)
import Data.Bifunctor (first)
import qualified Data.Char as Char
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

{-# ANN module ("DisableMutations" :: String) #-}

newtype Tag = Tag {tagText :: Text}
  deriving stock (Eq, Ord, Generic, Lift)
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
  codec = bimapCodec (first renderInvalid . Centjes.Tag.fromText) toText codec

instance IsString Tag where
  fromString s = case Centjes.Tag.fromString s of
    Left err -> error (renderInvalid err)
    Right t -> t

fromString :: String -> Either [ValidationChain] Tag
fromString = fromText . T.pack

fromText :: Text -> Either [ValidationChain] Tag
fromText = checkValidity . Tag

toText :: Tag -> Text
toText = tagText

toString :: Tag -> String
toString = T.unpack . toText

isTypoOf :: Tag -> Tag -> Bool
isTypoOf t1 t2 = Text.isTypoOf (toText t1) (toText t2)
