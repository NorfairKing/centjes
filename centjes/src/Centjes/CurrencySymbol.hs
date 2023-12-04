{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.CurrencySymbol
  ( CurrencySymbol (..),
    parseCurrencySymbolM,
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

-- TODO rename to currencySymbolText
newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol :: Text}
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

instance NFData CurrencySymbol

parseCurrencySymbolM :: MonadFail m => Text -> m CurrencySymbol
parseCurrencySymbolM t = case parseCurrencySymbol t of
  Nothing -> fail $ "Invalid currency symbol: " <> show t
  Just cs -> pure cs

parseCurrencySymbol :: Text -> Maybe CurrencySymbol
parseCurrencySymbol = constructValid . CurrencySymbol
