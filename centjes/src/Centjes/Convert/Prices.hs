{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert.Prices
  ( Prices (..),
    FromTo (..),
    empty,
    singleton,
    insert,
    fromList,
    lookupConversionFactor,
  )
where

import Centjes.Ledger (Currency (currencyQuantisationFactor))
import Centjes.Location
import Control.Monad
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Sequence (Seq, ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics (Generic)
import Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.QuantisationFactor as Money (QuantisationFactor)

-- | A price graph:
-- Map from edge to rate
--
-- TODO: Rename to PriceGraph
newtype Prices cur = Prices {unPrices :: Map (FromTo cur) ConversionRate}
  deriving (Show, Eq, Generic)

instance (Validity cur, Show cur, Ord cur) => Validity (Prices cur)

-- Strict, ordered pair to represent an undirected edge
data FromTo cur = FromTo !cur !cur
  deriving (Show, Eq, Ord, Generic)

instance (Validity cur, Ord cur) => Validity (FromTo cur) where
  validate ft@(FromTo from to) =
    mconcat
      [ genericValidate ft,
        declare "from is not equal to to" $
          from /= to,
        declare "from is less than to" $
          from <= to
      ]

fromToInverted :: Ord a => a -> a -> Bool
fromToInverted = (>)

mkFromTo :: Ord a => a -> a -> FromTo a
mkFromTo a1 a2 = snd $ mkFromTo' a1 a2

mkFromTo' :: Ord a => a -> a -> (Money.ConversionRate -> Money.ConversionRate, FromTo a)
mkFromTo' a1 a2 =
  if fromToInverted a1 a2
    then (ConversionRate.invert, FromTo a2 a1)
    else (id, FromTo a1 a2)

fromToSet :: Ord a => FromTo a -> Set a
fromToSet (FromTo a1 a2) = S.fromList [a1, a2]

empty :: Prices cur
empty = Prices M.empty

singleton :: Ord cur => cur -> cur -> Money.ConversionRate -> Prices cur
singleton from to rate =
  if from == to
    then empty
    else
      let (f, fromTo) = mkFromTo' from to
       in Prices $ M.singleton fromTo (f rate)

-- TODO precompute this?
currencies :: Ord cur => Prices cur -> Set cur
currencies (Prices m) = S.unions $ map fromToSet $ M.keys m

insert :: forall cur. Ord cur => cur -> cur -> Money.ConversionRate -> Prices cur -> Prices cur
insert from to rate p@(Prices m) =
  if from == to
    then p
    else
      let (f, fromTo@(FromTo from' to')) = mkFromTo' from to
       in Prices $ M.insert fromTo (f rate) m

fromList :: Ord cur => [((cur, cur), Money.ConversionRate)] -> Prices cur
fromList = foldl' (\ps ((c1, c2), r) -> insert c1 c2 r ps) empty

lookupConversionFactor :: forall cur. Ord cur => Prices cur -> cur -> cur -> Maybe Money.ConversionRate
lookupConversionFactor (Prices m) from to =
  go ConversionRate.oneToOne
    <$> breadthFirstSearch edgesFrom from to
  where
    go r = \case
      PathEnd _ -> r
      PathFrom _ r' p -> go (ConversionRate.compose r r') p
    -- TODO this can probably be faster by using a better structure
    edgesFrom :: cur -> Map cur Money.ConversionRate
    edgesFrom n =
      M.fromList $
        mapMaybe
          ( \(FromTo f t, rate) ->
              if f == n
                then Just (t, rate)
                else
                  if t == n
                    then Just (f, ConversionRate.invert rate)
                    else Nothing
          )
          (M.toList m)

breadthFirstSearch ::
  Ord node =>
  (node -> Map node edge) ->
  node ->
  node ->
  Maybe (Path node edge)
breadthFirstSearch getEdges start goal =
  -- Start from the goal and go to the start so the path is not reversed.
  go (S.singleton goal) (Seq.singleton (PathEnd goal))
  where
    go visited queue = case Seq.viewl queue of
      -- If the queue is empty, there's no path from the goal to the start
      EmptyL -> Nothing
      (currentPath :< restQueue) ->
        -- If the head is the starting node, this is path to the goal and we're
        -- done.
        let currentNode = pathHead currentPath
         in if currentNode == start
              then Just currentPath
              else
                let considerEdge (node, edge) =
                      -- If this node is already explored, don't consider this edge
                      if S.member node visited
                        then Nothing
                        else do
                          -- Consider this node explored now and
                          -- make a path and add it to the queue
                          let p = PathFrom node edge currentPath
                          Just (node, p)

                    tups = mapMaybe considerEdge (M.toList (getEdges currentNode))
                    newVisited = foldl' (\v (node, _) -> S.insert node v) visited tups
                    newQueue = foldl' (\q (_, path) -> q |> path) restQueue tups
                 in go newVisited newQueue

-- Path from start to goal
data Path node edge
  = PathEnd node
  | PathFrom node edge (Path node edge)

-- | The tip of the path
pathHead :: Path node edge -> node
pathHead = \case
  PathEnd cur -> cur
  PathFrom cur _ _ -> cur
