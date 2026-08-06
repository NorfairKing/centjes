{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Comment
  ( Comment (..),
    commentLines,
    fromText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)

{-# ANN module ("DisableMutations" :: String) #-}

-- | The text of a comment: its lines joined by newlines, each without the @--@
-- that introduces it.
--
-- A run of comment lines is one comment.
newtype Comment = Comment {unComment :: Text}
  deriving stock (Show, Eq, Generic)

instance Validity Comment where
  validate c@(Comment t) =
    mconcat
      [ genericValidate c,
        -- The lexer ends a line on these too, and 'commentLines' does not split
        -- on them, so a comment containing one could not be read back.
        declare "The comment contains no carriage returns or form feeds" $
          not (T.any (\char -> char == '\r' || char == '\f') t)
      ]

-- | The lines of a comment, one per @--@ line it was written as.
--
-- Note: This splits rather than using 'Data.Text.lines', which is not the
-- inverse of the join: it drops a trailing empty line, so a comment of two
-- empty lines would come back as one.
commentLines :: Comment -> [Text]
commentLines = T.splitOn "\n" . unComment

fromText :: Text -> Either String Comment
fromText = prettyValidate . Comment
