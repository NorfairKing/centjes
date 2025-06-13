{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Filter
  ( Filter (..),
    predicate,
  )
where

import Autodocodec
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

instance HasCodec Filter where
  codec = named "Filter" $ dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left "any" -> FilterAny
        Left s -> FilterSubstring s
        Right fs -> FilterOr fs
      g = \case
        FilterAny -> Left "any"
        FilterSubstring s -> Left s
        FilterOr fs -> Right fs

instance HasParser Filter where
  settingsParser =
    withDefault FilterAny $
      choice
        [ FilterOr
            <$> someNonEmpty
              ( FilterSubstring
                  <$> setting
                    [ help "filter",
                      reader $ eitherReader $ \s -> case s of
                        '-' : _ -> Left "Filters must not start with a dash"
                        _ -> Right (T.pack s),
                      argument,
                      metavar "FILTER"
                    ]
              ),
          setting
            [ help "filter",
              conf "filter",
              metavar "FILTER"
            ],
          pure FilterAny
        ]

predicate :: Filter -> (AccountName -> Bool)
predicate = \case
  FilterAny -> const True
  FilterSubstring t -> T.isInfixOf t . AccountName.toText
  FilterOr fs -> \an -> any (`predicate` an) fs
