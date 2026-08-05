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

{-# ANN module ("DisableMutations" :: String) #-}

newtype Description = Description {unDescription :: Text}
  deriving stock (Show, Eq, Generic)

-- A description is written as one or more @|@ lines, each with text after the
-- @|@, so a description is exactly its lines joined by newlines, with every
-- line non-empty.  Anything else cannot be written down.
instance Validity Description where
  validate d@(Description t) =
    -- Note: This splits rather than using 'T.lines', which is not the inverse of
    -- the join: it drops a trailing empty line, so a trailing newline would go
    -- unnoticed here.
    let ls = T.splitOn "\n" t
     in mconcat
          [ genericValidate d,
            declare "The description is not empty" $ not (T.null t),
            declare "None of the lines of the description are empty" $
              not (any T.null ls),
            -- The lexer ends a line on a carriage return as well, but splitting
            -- on newlines does not split on one.
            declare "The description contains no carriage returns" $
              not (T.any (== '\r') t)
          ]

instance Semigroup Description where
  (<>) (Description t1) (Description t2) = Description $ t1 <> "\n" <> t2

fromTextM :: (MonadFail m) => Text -> m Description
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
