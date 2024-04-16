module Centjes.Parse.Utils where

import Centjes.Module
import Centjes.Parse.Alex
import Path

parseImportFrom :: String -> Alex Import
parseImportFrom s = fmap Import $ do
  rp <- maybeParser "relfile path" parseRelFile s
  case fileExtension rp of
    Just ".cent" -> pure rp
    Nothing -> case addExtension ".cent" rp of
      Nothing -> parseError "Should not happen; failed to add the .cent extension."
      Just rp' -> pure rp'
    Just _ -> parseError "Can only import .cent files."

parseError :: String -> Alex a
parseError = alexError
