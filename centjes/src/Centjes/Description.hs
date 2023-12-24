{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Description
  ( Description (..),
    nullDescription,
    fromTextM,
    fromText,
    toText,
    combine,
  )
where

import Control.DeepSeq
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Description = Description {unDescription :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Semigroup, Monoid, IsString)

instance Validity Description

instance NFData Description

nullDescription :: Description -> Bool
nullDescription = T.null . unDescription

fromTextM :: MonadFail m => Text -> m Description
fromTextM t = case fromText t of
  Nothing -> fail $ "Invalid description: " <> show t
  Just d -> pure d

fromText :: Text -> Maybe Description
fromText = constructValid . Description

toText :: Description -> Text
toText = unDescription

combine :: [Description] -> Maybe Description
combine = \case
  [] -> Nothing
  ds -> Just $ Description $ T.intercalate "\n" $ map unDescription ds
