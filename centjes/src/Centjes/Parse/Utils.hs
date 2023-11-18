module Centjes.Parse.Utils where

import Centjes.Module
import Centjes.Parse.Alex
import Data.Time
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

timeParser :: ParseTime t => String -> String -> Alex t
timeParser formatString s = case parseTimeM False defaultTimeLocale formatString s of
  Nothing -> parseError $ "Failed to parse time value: " <> formatString
  Just t -> pure t

maybeParser :: Show b => String -> (b -> Maybe a) -> b -> Alex a
maybeParser name func b =
  case func b of
    Nothing -> parseError $ "Failed to parse " <> name <> " from " <> show b
    Just a -> pure a

parseError :: String -> Alex a
parseError = alexError
