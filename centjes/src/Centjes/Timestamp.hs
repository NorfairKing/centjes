{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Timestamp
  ( Timestamp (..),
    MinuteOfDay (..),
    SecondOfDay (..),
    comparePartially,
    fromString,
    fromText,
    toString,
    toText,
    minuteFromLocalTime,
    secondFromLocalTime,
  )
where

import Control.Applicative
import Control.DeepSeq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Validity
import Data.Validity.Path ()
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Word
import GHC.Generics (Generic)

data Timestamp
  = TimestampDay !Day
  | TimestampMinute !Day !MinuteOfDay
  | TimestampSecond !Day !SecondOfDay
  deriving (Show, Eq, Generic)

instance Validity Timestamp

instance NFData Timestamp

comparePartially :: Timestamp -> Timestamp -> Maybe Ordering
comparePartially ts1 ts2 = case (ts1, ts2) of
  (TimestampDay d1, TimestampDay d2) -> Just $ compare d1 d2
  (TimestampDay d1, TimestampMinute d2 _) -> case compare d1 d2 of
    EQ -> Nothing
    c -> Just c
  (TimestampDay d1, TimestampSecond d2 _) -> case compare d1 d2 of
    EQ -> Nothing
    c -> Just c
  (TimestampMinute d1 _, TimestampDay d2) -> case compare d1 d2 of
    EQ -> Nothing
    c -> Just c
  (TimestampMinute d1 m1, TimestampMinute d2 m2) -> Just $ compare d1 d2 <> compare m1 m2
  (TimestampMinute d1 (MinuteOfDay m1), TimestampSecond d2 (SecondOfDay s2)) ->
    case compare d1 d2 of
      EQ -> case compare m1 (fromIntegral (s2 `div` 60)) of
        EQ -> Nothing
        c -> Just c
      c -> Just c
  (TimestampSecond d1 _, TimestampDay d2) -> case compare d1 d2 of
    EQ -> Nothing
    c -> Just c
  (TimestampSecond d1 (SecondOfDay s1), TimestampMinute d2 (MinuteOfDay m2)) ->
    case compare d1 d2 of
      EQ -> case compare (fromIntegral (s1 `div` 60)) m2 of
        EQ -> Nothing
        c -> Just c
      c -> Just c
  (TimestampSecond d1 s1, TimestampSecond d2 s2) -> Just $ compare d1 d2 <> compare s1 s2

toText :: Timestamp -> Text
toText = T.pack . toString

toString :: Timestamp -> String
toString ts =
  case ts of
    TimestampDay d -> formatTime defaultTimeLocale "%F" d
    TimestampMinute d m -> formatTime defaultTimeLocale "%F %H:%M" (minuteToLocalTime d m)
    TimestampSecond d s -> formatTime defaultTimeLocale "%F %H:%M:%S" (secondToLocalTime d s)

fromString :: String -> Either String Timestamp
fromString s =
  (secondFromLocalTime <$> parseTimeEither defaultTimeLocale "%F %H:%M:%S" s)
    <|> (minuteFromLocalTime <$> parseTimeEither defaultTimeLocale "%F %H:%M" s)
    <|> (TimestampDay <$> parseTimeEither defaultTimeLocale "%F" s)

minuteFromLocalTime :: LocalTime -> Timestamp
minuteFromLocalTime (LocalTime d tod) = TimestampMinute d (MinuteOfDay (floor (timeOfDayToTime tod / 60)))

secondFromLocalTime :: LocalTime -> Timestamp
secondFromLocalTime (LocalTime d tod) = TimestampSecond d (SecondOfDay (floor (timeOfDayToTime tod)))

fromText :: Text -> Either String Timestamp
fromText = fromString . T.unpack

parseTimeEither :: ParseTime a => TimeLocale -> String -> String -> Either String a
parseTimeEither locale format string = case parseTimeM True locale format string of
  Nothing -> Left $ "Failed to parse time value: " <> string <> " via " <> format
  Just r -> Right r

newtype MinuteOfDay = MinuteOfDay {unMinuteOfDay :: Word16}
  deriving (Show, Eq, Ord, Generic)

instance Validity MinuteOfDay where
  validate m@(MinuteOfDay ms) =
    mconcat
      [ genericValidate m,
        declare "The number of minutes is under 24*60" $
          ms < 24 * 60
      ]

instance NFData MinuteOfDay

minuteToLocalTime :: Day -> MinuteOfDay -> LocalTime
minuteToLocalTime d (MinuteOfDay ms) = LocalTime d $ timeToTimeOfDay $ fromIntegral ms * 60

newtype SecondOfDay = SecondOfDay {unSecondOfDay :: Word32}
  deriving (Show, Eq, Ord, Generic)

instance Validity SecondOfDay where
  validate m@(SecondOfDay ms) =
    mconcat
      [ genericValidate m,
        declare "The number of seconds is under 24*60*60" $
          ms < 24 * 60 * 60
      ]

instance NFData SecondOfDay

secondToLocalTime :: Day -> SecondOfDay -> LocalTime
secondToLocalTime d (SecondOfDay ms) = LocalTime d $ timeToTimeOfDay $ fromIntegral ms
