{-# LANGUAGE OverloadedStrings #-}

module Centjes.Format
  ( formatModule,
    formatDeclaration,
    formatTransaction,
    formatPosting,
  )
where

import Centjes.Module
import Data.Text (Text)

formatModule :: Module -> Text
formatModule _ = ""

formatDeclaration :: Declaration -> Text
formatDeclaration _ = ""

formatTransaction :: Transaction -> Text
formatTransaction _ = ""

formatPosting :: Posting -> Text
formatPosting _ = ""
