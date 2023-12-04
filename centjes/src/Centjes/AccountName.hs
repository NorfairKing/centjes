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
        decorateText t $ \c -> declare "The character is a latin1 alphanumeric character or _, - or :" $
          case c of
            ':' -> True
            '-' -> True
            '_' -> True
            _
              | Char.isLatin1 c && Char.isAlphaNum c -> True
              | otherwise -> False
      ]

instance NFData AccountName

instance HasCodec AccountName where
  codec = bimapCodec f accountNameText codec
    where
      f t = case fromText t of
        Nothing -> Left $ "Invalid AccountName: " <> show t
        Just an -> Right an

fromText :: Text -> Maybe AccountName
fromText = constructValid . AccountName
