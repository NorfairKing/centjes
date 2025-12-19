{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.AccountType
  ( AccountType (..),
    fromText,
    toText,
    toString,
    assertion,
    fromAccountName,
  )
where

import Centjes.AccountName (AccountName)
import qualified Centjes.AccountName as AccountName
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)

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
  | -- | No assertions
    AccountTypeOther
  deriving stock (Show, Eq, Generic)

instance Validity AccountType

fromText :: Text -> Maybe AccountType
fromText = \case
  "assets" -> Just AccountTypeAssets
  "liabilities" -> Just AccountTypeLiabilities
  "equity" -> Just AccountTypeEquity
  "expenses" -> Just AccountTypeExpenses
  "income" -> Just AccountTypeIncome
  "other" -> Just AccountTypeOther
  _ -> Nothing

-- | Prefer 'toText' over 'toString'.
toText :: AccountType -> Text
toText = \case
  AccountTypeAssets -> "assets"
  AccountTypeLiabilities -> "liabilities"
  AccountTypeEquity -> "equity"
  AccountTypeExpenses -> "expenses"
  AccountTypeIncome -> "income"
  AccountTypeOther -> "other"

-- | Prefer 'toText' over 'toString'.
toString :: AccountType -> String
toString = T.unpack . toText

assertion :: AccountType -> (Money.Account -> Bool)
assertion = \case
  AccountTypeAssets -> (>= Account.zero)
  AccountTypeLiabilities -> (<= Account.zero)
  AccountTypeEquity -> (<= Account.zero)
  AccountTypeExpenses -> (>= Account.zero)
  AccountTypeIncome -> (<= Account.zero)
  AccountTypeOther -> const True

fromAccountName :: AccountName -> Maybe AccountType
fromAccountName an =
  go AccountTypeAssets "assets"
    <|> go AccountTypeLiabilities "liabilities"
    <|> go AccountTypeEquity "equity"
    <|> go AccountTypeExpenses "expenses"
    <|> go AccountTypeIncome "income"
    <|> go AccountTypeOther "other"
  where
    go :: AccountType -> Text -> Maybe AccountType
    go t substring =
      if substring `T.isInfixOf` AccountName.toText an
        then Just t
        else Nothing
