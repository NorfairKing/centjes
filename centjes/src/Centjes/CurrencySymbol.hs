{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.CurrencySymbol
  ( CurrencySymbol (..),
    fromTextM,
    fromText,
    toText,
    toString,
  )
where

import Autodocodec
import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype CurrencySymbol = CurrencySymbol {currencySymbolText :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity CurrencySymbol where
  validate an@(CurrencySymbol t) =
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

instance HasCodec CurrencySymbol where
  codec = bimapCodec fromText currencySymbolText codec

fromTextM :: MonadFail m => Text -> m CurrencySymbol
fromTextM t = case fromText t of
  Left e -> fail $ unlines [unwords ["Invalid currency symbol:", show t], e]
  Right cs -> pure cs

fromText :: Text -> Either String CurrencySymbol
fromText = prettyValidate . CurrencySymbol

toText :: CurrencySymbol -> Text
toText = currencySymbolText

toString :: CurrencySymbol -> String
toString = T.unpack . toText
