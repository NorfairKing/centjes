{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Block
  ( BlockSize (..),
    Block (..),
    timestampBlockTitle,
    dayBlockTitle,
    renderBlockTitle,
    nextBlock,
  )
where

import Autodocodec
import Centjes.Timestamp (Timestamp)
import qualified Centjes.Timestamp as Timestamp
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Time.Calendar.WeekDate
import Data.Validity
import Data.Validity.Time ()
import GHC.Generics (Generic)
import OptEnvConf
import Text.Printf (printf)

data BlockSize
  = BlockSizeIndividual
  | BlockSizeDay
  | BlockSizeWeek
  | BlockSizeMonth
  | BlockSizeQuarter
  | BlockSizeYear
  deriving (Show, Generic)

instance Validity BlockSize

instance HasCodec BlockSize where
  codec = bimapCodec parseBlockSize renderBlockSize codec

parseBlockSize :: String -> Either String BlockSize
parseBlockSize = \case
  "individually" -> Right BlockSizeIndividual
  "daily" -> Right BlockSizeDay
  "weekly" -> Right BlockSizeWeek
  "monthly" -> Right BlockSizeMonth
  "quarterly" -> Right BlockSizeQuarter
  "yearly" -> Right BlockSizeYear
  s -> Left $ "Unknown block size: " <> show s

renderBlockSize :: BlockSize -> String
renderBlockSize = \case
  BlockSizeIndividual -> "individually"
  BlockSizeDay -> "daily"
  BlockSizeWeek -> "weekly"
  BlockSizeMonth -> "monthly"
  BlockSizeQuarter -> "quarterly"
  BlockSizeYear -> "yearly"

instance HasParser BlockSize where
  settingsParser =
    withDefault BlockSizeIndividual $
      choice
        [ setting
            [ help "All transactions in one block",
              switch BlockSizeIndividual,
              long "individually"
            ],
          setting
            [ help "Group transactions by day",
              switch BlockSizeDay,
              long "daily"
            ],
          setting
            [ help "Group transactions by week",
              switch BlockSizeWeek,
              long "weekly"
            ],
          setting
            [ help "Group transactions by month",
              switch BlockSizeMonth,
              long "monthly"
            ],
          setting
            [ help "Group transactions by quarter",
              switch BlockSizeQuarter,
              long "quarterly"
            ],
          setting
            [ help "Group transactions by year",
              switch BlockSizeYear,
              long "yearly"
            ],
          setting
            [ help "Group transactions",
              option,
              long "block",
              conf "block",
              reader $ eitherReader parseBlockSize,
              metavar "BLOCK_SIZE"
            ]
        ]

data Block
  = BlockIndividual
  | BlockDay !Day
  | BlockWeek !Year !WeekOfYear
  | BlockMonth !Month
  | BlockQuarter !Quarter
  | BlockYear !Year
  deriving (Show, Eq, Ord, Generic)

instance Validity Block

-- TODO upstream these
instance Validity Month where
  validate = trivialValidation

instance Validity Quarter where
  validate = trivialValidation

timestampBlockTitle :: Timestamp -> BlockSize -> Block
timestampBlockTitle ts =
  let d = Timestamp.toDay ts
   in dayBlockTitle d

dayBlockTitle :: Day -> BlockSize -> Block
dayBlockTitle d = \case
  BlockSizeIndividual -> BlockIndividual
  BlockSizeDay -> BlockDay d
  BlockSizeWeek ->
    let (y, woy, _) = toWeekDate d
     in BlockWeek y woy
  BlockSizeMonth -> BlockMonth $ dayPeriod d
  BlockSizeQuarter -> BlockQuarter $ dayPeriod d
  BlockSizeYear ->
    let (y, _, _) = toGregorian d
     in BlockYear y

renderBlockTitle :: Block -> Text
renderBlockTitle = \case
  BlockIndividual -> T.empty
  BlockDay d -> T.pack $ show d
  BlockWeek y woy -> T.pack $ concat [show y, "-W", printf "%02d" woy]
  BlockMonth m -> T.pack $ show m
  BlockQuarter q -> T.pack $ show q
  BlockYear y -> T.pack $ show y

nextBlock :: Block -> Block
nextBlock = \case
  BlockIndividual -> BlockIndividual
  BlockDay d -> BlockDay $ succ d
  BlockWeek y woy ->
    let d = fromWeekDate y woy 1
        d' = addDays 7 d
        (y', woy', _) = toWeekDate d'
     in BlockWeek y' woy'
  BlockMonth m -> BlockMonth $ succ m
  BlockQuarter q -> BlockQuarter $ succ q
  BlockYear y -> BlockYear $ succ y
