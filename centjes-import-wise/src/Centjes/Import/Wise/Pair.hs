{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Putting the two sides of a conversion back together.
--
-- A conversion moves money between two of your own balances, so Wise writes it
-- into the statement of both of them.  Read one statement on its own and a
-- conversion looks like money vanishing; read both and it is one transaction
-- with a currency on each side.
module Centjes.Import.Wise.Pair
  ( WiseEvent (..),
    wiseEventDay,
    wiseEventId,
    PairError (..),
    renderPairError,
    pairRows,
  )
where

import Centjes.CurrencySymbol (CurrencySymbol (..))
import Centjes.Import.Wise.Statement
import Data.Either (lefts, rights)
import Data.List (sortOn, tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Numeric.DecimalLiteral as DecimalLiteral

-- | What one or two statement rows amount to.
data WiseEvent
  = -- | Money entering or leaving the account.
    WiseMovement !Row
  | -- | Money moving between two of your own balances: the leg it left, then
    -- the leg it arrived in.
    WiseConversion !Row !Row
  deriving (Show, Eq)

wiseEventDay :: WiseEvent -> Day
wiseEventDay = \case
  WiseMovement row -> rowDate row
  WiseConversion out _ -> rowDate out

wiseEventId :: WiseEvent -> Text
wiseEventId = \case
  WiseMovement row -> rowId row
  WiseConversion out _ -> rowId out

data PairError
  = -- | A conversion leg whose counterpart is in a statement that was not read.
    PairErrorUnpairedConversion !Row
  | -- | The same row twice, which is what passing one statement twice looks
    -- like.
    PairErrorDuplicateRow !Row
  | -- | Statements that disagree about what happened first.
    PairErrorOrderConflict ![Text]
  | -- | Rows sharing an id that are not a conversion and not duplicates.
    PairErrorUnexpectedGroup !Text !Int
  deriving (Show, Eq)

renderPairError :: PairError -> String
renderPairError = \case
  PairErrorUnpairedConversion row ->
    unlines
      [ unwords
          [ "The conversion",
            show (T.unpack (rowId row)),
            "on",
            formatTime defaultTimeLocale "%F" (rowDate row),
            "has only one side in the statements that were read."
          ],
        unwords
          [ "Pass the statement for the",
            maybe "other" (T.unpack . currencySymbolText) (otherSideCurrency row),
            "balance as well."
          ]
      ]
  PairErrorDuplicateRow row ->
    unwords
      [ "The row",
        show (T.unpack (rowId row)),
        "in",
        T.unpack (currencySymbolText (rowCurrency row)),
        "appears twice.  Was the same statement passed more than once?"
      ]
  PairErrorOrderConflict transferIds ->
    unlines
      [ "The statements disagree about the order these happened in:",
        unwords (map (show . T.unpack) transferIds),
        unwords
          [ "A conversion is in two statements at once, so the two have to agree",
            "on what came before it."
          ]
      ]
  PairErrorUnexpectedGroup transferId n ->
    unwords
      [ "Found",
        show n,
        "rows sharing the id",
        show (T.unpack transferId) <> ",",
        "which is neither one movement nor the two sides of a conversion.",
        "This importer reads COMPACT statements, which have one row per transaction."
      ]

-- | The currency of the statement that would hold this leg's counterpart.
otherSideCurrency :: Row -> Maybe CurrencySymbol
otherSideCurrency row = do
  from <- rowExchangeFrom row
  to <- rowExchangeTo row
  pure $ if rowCurrency row == from then to else from

-- | Turn the rows of every statement that was read into what happened.
--
-- Rows are matched by their Wise id, which is the same on both sides of a
-- conversion.
pairRows :: [Row] -> Either [PairError] [WiseEvent]
pairRows rows =
  let grouped = M.toList (M.fromListWith (flip (++)) [(rowId row, [row]) | row <- rows])
      results = map (uncurry groupEvents) grouped
      errors = concat (lefts results)
   in if not (null errors)
        then Left errors
        else orderEvents rows (concat (rights results))

-- | Put the events in the order they have to be written.
--
-- Which of two things on the same day happened first is not something the date
-- column says, but a statement lists its own balance in the order they
-- happened, and the running balance it states only adds up in that order.  So
-- what comes out is an order that agrees with every statement at once: sorting
-- by anything else can put a conversion before the money that paid for it, and
-- then every assertion after it is wrong.
--
-- Within that, the day and the id decide, so that the same statements always
-- give the same file.
orderEvents :: [Row] -> [WiseEvent] -> Either [PairError] [WiseEvent]
orderEvents rows events =
  let numbered = zip [0 :: Int ..] events
      eventsByIndex = M.fromList numbered
      eventOfRow =
        M.fromList
          [ ((rowCurrency row, rowId row), i)
          | (i, event) <- numbered,
            row <- eventRows event
          ]
      -- Each balance's statement, as the events it is made of, in its order.
      --
      -- Sorted by day as well as taken in the order the rows came in, so that
      -- statements that were read out of order, or one balance exported in
      -- several pieces, still say the same thing.  The sort is stable, so
      -- within a day the statement's own order is what is kept, which is the
      -- only thing that says what happened first.
      chains =
        map (map snd . sortOn fst) $
          M.elems $
            M.fromListWith
              (flip (++))
              [ (rowCurrency row, [(rowDate row, i)])
              | row <- rows,
                Just i <- [M.lookup (rowCurrency row, rowId row) eventOfRow]
              ]
      successors = M.fromListWith (++) [(from, [to]) | chain <- chains, (from, to) <- zip chain (drop 1 chain)]
      indegrees =
        M.fromListWith
          (+)
          ( [(i, 0 :: Int) | (i, _) <- numbered]
              ++ [(to, 1) | chain <- chains, to <- drop 1 chain]
          )
      sortKey i = case M.lookup i eventsByIndex of
        Nothing -> Nothing
        Just event -> Just (wiseEventDay event, wiseEventId event, i)
      ready = S.fromList [key | (i, degree) <- M.toList indegrees, degree == 0, Just key <- [sortKey i]]
      ordered = untangle successors sortKey ready indegrees []
   in if length ordered == length events
        then Right [event | i <- ordered, Just event <- [M.lookup i eventsByIndex]]
        else
          Left
            [ PairErrorOrderConflict
                [wiseEventId event | (i, event) <- numbered, i `notElem` ordered]
            ]

-- | Kahn's algorithm, taking whichever of the events that could come next comes
-- first by day and then by id.
untangle ::
  Map Int [Int] ->
  (Int -> Maybe (Day, Text, Int)) ->
  Set (Day, Text, Int) ->
  Map Int Int ->
  [Int] ->
  [Int]
untangle successors sortKey ready indegrees acc = case S.minView ready of
  Nothing -> reverse acc
  Just ((_, _, i), rest) ->
    let afterThis = M.findWithDefault [] i successors
        indegrees' = foldr (M.adjust (subtract 1)) indegrees afterThis
        freed =
          S.fromList
            [ key
            | j <- afterThis,
              M.findWithDefault 1 j indegrees' == 0,
              Just key <- [sortKey j]
            ]
     in untangle successors sortKey (S.union rest freed) indegrees' (i : acc)

eventRows :: WiseEvent -> [Row]
eventRows = \case
  WiseMovement row -> [row]
  WiseConversion out back -> [out, back]

groupEvents :: Text -> [Row] -> Either [PairError] [WiseEvent]
groupEvents transferId rowsWithThisId =
  case duplicatedRows rowsWithThisId of
    (duplicate : _) -> Left [PairErrorDuplicateRow duplicate]
    [] -> case rowsWithThisId of
      [] -> Right []
      [row]
        | rowIsConversionLeg row -> Left [PairErrorUnpairedConversion row]
        | otherwise -> Right [WiseMovement row]
      [first, second]
        | rowIsConversionLeg first,
          rowIsConversionLeg second,
          rowCurrency first /= rowCurrency second,
          Just (out, back) <- orderLegs first second ->
            Right [WiseConversion out back]
      _
        | any rowIsConversionLeg rowsWithThisId ->
            Left [PairErrorUnexpectedGroup transferId (length rowsWithThisId)]
        | otherwise -> Right (map WiseMovement rowsWithThisId)

-- | Every row that appears more than once in the group.
duplicatedRows :: [Row] -> [Row]
duplicatedRows rows =
  [ row
  | (row, later) <- zip rows (drop 1 (tails rows)),
    row `elem` later
  ]

-- | Which leg money left and which it arrived in, or nothing if they do not
-- look like two sides of one conversion.
orderLegs :: Row -> Row -> Maybe (Row, Row)
orderLegs first second =
  case (rowIsNegative first, rowIsNegative second) of
    (True, False) -> Just (first, second)
    (False, True) -> Just (second, first)
    _ -> Nothing

rowIsNegative :: Row -> Bool
rowIsNegative row = DecimalLiteral.toRational (rowAmount row) < 0
