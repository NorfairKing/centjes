{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.AccountName
  ( AccountName (..),
    fromText,
  )
where

import Autodocodec
import Control.DeepSeq
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)

-- TODO Rename accountNameText to accountNameText
newtype AccountName = AccountName {accountNameText :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity AccountName where
  validate an@(AccountName t) =
    mconcat
      [ genericValidate an,
        declare "The account name is not empty" $ not (T.null t),
        declare "The account name starts with an alphabetic character" $ case T.uncons t of
          Nothing -> False
          Just (h, _) -> Char.isAlpha h,
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character or _ or :" $
          case c of
            ':' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

instance NFData AccountName

instance HasCodec AccountName where
  -- TODO actual parsing
  codec = dimapCodec AccountName accountNameText codec

fromText :: Text -> Maybe AccountName
fromText = constructValid . AccountName
