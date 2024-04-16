{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Description
  ( Description (..),
    fromTextM,
    fromText,
    toText,
    combine,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)

newtype Description = Description {unDescription :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity Description

fromTextM :: MonadFail m => Text -> m Description
fromTextM t = case fromText t of
  Left err -> fail $ unlines ["Invalid description:", show t, err]
  Right d -> pure d

fromText :: Text -> Either String Description
fromText = prettyValidate . Description

toText :: Description -> Text
toText = unDescription

combine :: [Description] -> Maybe Description
combine = \case
  [] -> Nothing
  ds -> Just $ Description $ T.intercalate "\n" $ map unDescription ds
