{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
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

-- | The segments of an account name, root first.
--
-- Root first, so that the derived 'Ord' puts every account right after its
-- parent.
newtype AccountName = AccountName {unAccountName :: NonEmpty Text}
  deriving stock (Eq, Ord, Generic, Lift)

-- [allow:HsNoCustomShowRead] opt-env-conf documents a setting's default with
-- show, and an account name should read there the way it is written in a
-- config file.
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
fromTextOrError = prettyValidate . AccountName . NE.fromList . T.splitOn ":"

-- | Prefer 'fromTextOrErr' over 'fromStringOrErr'
fromStringOrError :: String -> Either String AccountName
fromStringOrError = fromTextOrError . T.pack

-- | Prefer 'toText' over 'toString'.
toText :: AccountName -> Text
toText = T.intercalate ":" . NE.toList . unAccountName

-- | Prefer 'toText' over 'toString'.
toString :: AccountName -> String
toString = T.unpack . toText

parent :: AccountName -> Maybe AccountName
parent (AccountName ts) = AccountName <$> NE.nonEmpty (NE.init ts)

ancestors :: AccountName -> [AccountName]
ancestors an = case parent an of
  Nothing -> []
  Just an' -> an' : ancestors an'

isTypoOf :: AccountName -> AccountName -> Bool
isTypoOf an1 an2 = Text.isTypoOf (toText an1) (toText an2)
