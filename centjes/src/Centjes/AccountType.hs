{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.AccountType
  ( AccountType (..),
    fromTextM,
    fromText,
    toText,
  )
where

import Control.DeepSeq
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)

data AccountType
  = -- | Never negative
    AccountTypeAssets
  | -- | Never positive
    AccountTypeLiabilities
  | -- | Never positive
    AccountTypeEquity
  | -- | Never negative
    AccountTypeExpenses
  | -- | Never positive
    AccountTypeIncome
  deriving stock (Show, Eq, Generic)

instance Validity AccountType

instance NFData AccountType

fromTextM :: MonadFail m => Text -> m AccountType
fromTextM t = case fromText t of
  Nothing -> fail $ "Unknown account type: " <> show t
  Just at -> pure at

fromText :: Text -> Maybe AccountType
fromText = \case
  "assets" -> Just AccountTypeAssets
  "liabilities" -> Just AccountTypeLiabilities
  "equity" -> Just AccountTypeEquity
  "expenses" -> Just AccountTypeExpenses
  "income" -> Just AccountTypeIncome
  _ -> Nothing

toText :: AccountType -> Text
toText = \case
  AccountTypeAssets -> "assets"
  AccountTypeLiabilities -> "liabilities"
  AccountTypeEquity -> "equity"
  AccountTypeExpenses -> "expenses"
  AccountTypeIncome -> "income"
