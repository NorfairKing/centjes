{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Centjes.Filter
  ( Filter (..),
    args,
    predicate,
  )
where

import Centjes.AccountName (AccountName (..))
import qualified Centjes.AccountName as AccountName
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
  deriving (Show, Generic)

instance Validity Filter

predicate :: Filter -> (AccountName -> Bool)
predicate = \case
  FilterAny -> const True
  FilterSubstring t -> T.isInfixOf t . AccountName.toText
  FilterOr fs -> \an -> any (`predicate` an) fs

args :: OptParse.Parser Filter
args =
  (FilterOr <$> some arg)
    <|> pure FilterAny

arg :: OptParse.Parser Filter
arg =
  FilterSubstring
    <$> strArgument
      ( mconcat
          [ help "filter",
            metavar "FILTER"
          ]
      )
