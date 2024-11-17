{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert.PriceGraph
  ( PriceGraph (..),
    empty,
    singleton,
    insert,
    fromList,
    lookup,
  )
where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import Prelude hiding (lookup)

-- | A price graph:
-- Map from node to a map of edges from that node.
-- Instead of saving undirected edges, we save directed edges twice each: one
-- with the rate and one with the reverse rate.
-- This way we use twice the memory, and take twice as long to insert, but lookups are cheap.
newtype PriceGraph priority cur = PriceGraph {unPriceGraph :: Map cur (Map cur (ConversionRate, priority))}
  deriving (Show, Eq, Generic)

instance (Validity priority, Show priority, Ord priority, Validity cur, Show cur, Ord cur) => Validity (PriceGraph priority cur)

empty :: PriceGraph priority cur
empty = PriceGraph M.empty

singleton :: (Ord cur) => cur -> cur -> Money.ConversionRate -> priority -> PriceGraph priority cur
singleton from to rate priority =
  if from == to
    then empty
    else insert from to rate priority empty

insert ::
  forall priority cur.
  (Ord cur) =>
  cur ->
  cur ->
  Money.ConversionRate ->
  priority ->
  PriceGraph priority cur ->
  PriceGraph priority cur
insert from to rate priority p@(PriceGraph m) =
  if from == to
    then p
    else
      PriceGraph $
        M.insertWith M.union to (M.singleton from (ConversionRate.invert rate, priority)) $
          M.insertWith M.union from (M.singleton to (rate, priority)) m

fromList :: (Ord cur) => [((cur, cur), (Money.ConversionRate, priority))] -> PriceGraph priority cur
fromList = foldl' (\ps ((c1, c2), (r, p)) -> insert c1 c2 r p ps) empty

lookup :: forall priority cur. (Ord priority, Ord cur) => PriceGraph priority cur -> cur -> cur -> Maybe Money.ConversionRate
lookup (PriceGraph m) from to =
  go ConversionRate.oneToOne
    <$> breadthFirstSearch edgesFrom from to
  where
    go r = \case
      PathStart _ -> r
      PathFrom _ r' p -> go (ConversionRate.compose r r') p
    edgesFrom :: cur -> Map cur (Money.ConversionRate, priority)
    edgesFrom n = fromMaybe M.empty $ M.lookup n m

-- Greatest priority is the largest
breadthFirstSearch ::
  forall node edge priority.
  (Ord node, Ord edge, Ord priority) =>
  (node -> Map node (edge, priority)) ->
  node ->
  node ->
  -- Reverse path from goal to start
  Maybe (Path node edge)
breadthFirstSearch getEdges start goal =
  go (S.singleton start) (PSQ.singleton (PathStart start) (0, Nothing) ())
  where
    go :: Set node -> OrdPSQ (Path node edge) (Int, Maybe (Down priority)) () -> Maybe (Path node edge)
    go visited queue = case PSQ.minView queue of
      -- If the queue is empty, there's no path from the goal to the start
      Nothing -> Nothing
      Just (currentPath, (curLen, mCurrentPriority), _, restQueue) ->
        -- If the head is the goal node, this is path to the start and we're
        -- done.
        let currentNode = pathHead currentPath
         in if currentNode == goal
              then Just currentPath
              else
                let considerEdge (v, q) (node, (edge, priority)) =
                      -- If this node is already explored, don't consider this edge
                      if S.member node visited
                        then (v, q)
                        else do
                          -- Consider this node explored now and
                          -- make a path and add it to the queue
                          let p = PathFrom node edge currentPath
                          let combinedPriority = case mCurrentPriority of
                                Nothing -> priority
                                Just (Down currentPriority) -> min currentPriority priority
                          (S.insert node v, PSQ.insert p (succ curLen, Just (Down combinedPriority)) () q)

                    (newVisited, newQueue) = foldl' considerEdge (visited, restQueue) (M.toList (getEdges currentNode))
                 in go newVisited newQueue

-- Path from start to goal
data Path node edge
  = PathStart node
  | PathFrom node edge (Path node edge)
  deriving (Eq, Ord)

-- | The tip of the path
pathHead :: Path node edge -> node
pathHead = \case
  PathStart cur -> cur
  PathFrom cur _ _ -> cur
