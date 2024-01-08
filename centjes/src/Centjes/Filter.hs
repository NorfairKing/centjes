{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Centjes.Filter
  ( Filter (..),
    filterArgs,
    filterPredicate,
  )
where

import Centjes.AccountName (AccountName (..))
import Control.DeepSeq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Options.Applicative as OptParse

data Filter
  = FilterAny
  | FilterOr [Filter]
  | FilterSubstring Text
  deriving (Show, Eq, Generic)

instance Validity Filter

instance NFData Filter

filterPredicate :: Filter -> (AccountName -> Bool)
filterPredicate = \case
  FilterAny -> const True
  FilterSubstring t -> \(AccountName ts) -> any (T.isInfixOf t) ts
  FilterOr fs -> \an -> any (`filterPredicate` an) fs

filterArgs :: OptParse.Parser Filter
filterArgs =
  (FilterOr <$> some filterArg)
    <|> pure FilterAny

filterArg :: OptParse.Parser Filter
filterArg =
  FilterSubstring
    <$> strArgument
      ( mconcat
          [ help "filter",
            metavar "FILTER"
          ]
      )
