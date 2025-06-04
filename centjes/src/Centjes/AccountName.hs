{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.AccountName
  ( AccountName (..),
    Centjes.AccountName.fromString,
    fromText,
    fromTextOrError,
    fromStringOrError,
    toString,
    toText,
    parent,
    ancestors,
    isTypoOf,
  )
where

import Autodocodec
import qualified Centjes.Typo as Text
import qualified Data.Char as Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype AccountName = AccountName {unAccountName :: NonEmpty Text}
  deriving stock (Eq, Generic)

instance Show AccountName where
  show = show . toString

instance Validity AccountName where
  validate an@(AccountName ts) =
    mconcat
      [ genericValidate an,
        decorateList (NE.toList ts) $ \t ->
          mconcat
            [ declare "The account name starts with an alphabetic character" $ case T.uncons t of
                Nothing -> False
                Just (h, _) -> Char.isAlpha h,
              decorateText t $ \c ->
                declare "The character is a latin1 alphanumeric character or _, -" $
                  validAccountNameChar c
            ]
      ]

validAccountNameChar :: Char -> Bool
validAccountNameChar = \case
  ':' -> False -- Separator character
  '-' -> True
  '_' -> True
  c
    | Char.isLatin1 c && Char.isAlphaNum c -> True
    | otherwise -> False

instance Ord AccountName where
  compare = comparing toText

instance HasCodec AccountName where
  codec = bimapCodec f toText codec
    where
      f t = case fromText t of
        Nothing -> Left $ "Invalid AccountName: " <> show t
        Just an -> Right an

instance IsString AccountName where
  fromString s = case fromText (T.pack s) of
    Nothing -> error $ "Invalid AccountName literal: " <> show s
    Just an -> an

fromText :: Text -> Maybe AccountName
fromText = either (const Nothing) Just . fromTextOrError

-- | Prefer 'fromText' over 'fromString'.
fromString :: String -> Maybe AccountName
fromString = fromText . T.pack

fromTextOrError :: Text -> Either String AccountName
fromTextOrError = prettyValidate . AccountName . NE.fromList . reverse . T.splitOn ":"

-- | Prefer 'fromTextOrErr' over 'fromStringOrErr'
fromStringOrError :: String -> Either String AccountName
fromStringOrError = fromTextOrError . T.pack

-- | Prefer 'toText' over 'toString'.
toText :: AccountName -> Text
toText = T.intercalate ":" . reverse . NE.toList . unAccountName

-- | Prefer 'toText' over 'toString'.
toString :: AccountName -> String
toString = T.unpack . toText

parent :: AccountName -> Maybe AccountName
parent (AccountName (_ :| ts)) = do
  ts' <- NE.nonEmpty ts
  pure $ AccountName ts'

ancestors :: AccountName -> [AccountName]
ancestors an = case parent an of
  Nothing -> []
  Just an' -> an' : ancestors an'

isTypoOf :: AccountName -> AccountName -> Bool
isTypoOf an1 an2 = Text.isTypoOf (toText an1) (toText an2)
