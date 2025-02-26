{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Centjes.Filter
  ( Filter (..),
    predicate,
  )
where

import Centjes.AccountName (AccountName (..))
import qualified Centjes.AccountName as AccountName
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import OptEnvConf

data Filter
  = FilterAny
  | FilterOr !(NonEmpty Filter)
  | FilterSubstring !Text
  deriving (Show, Generic)

instance Validity Filter

instance HasParser Filter where
  settingsParser =
    choice
      [ FilterOr
          <$> someNonEmpty
            ( FilterSubstring
                <$> setting
                  [ help "filter",
                    reader str,
                    argument,
                    metavar "FILTER"
                  ]
            ),
        pure FilterAny
      ]

predicate :: Filter -> (AccountName -> Bool)
predicate = \case
  FilterAny -> const True
  FilterSubstring t -> T.isInfixOf t . AccountName.toText
  FilterOr fs -> \an -> any (`predicate` an) fs
