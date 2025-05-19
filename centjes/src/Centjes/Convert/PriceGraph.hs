{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert.PriceGraph
  ( PriceGraph (..),
    Direction (..),
    empty,
    singleton,
    insert,
    fromList,
    lookup,
    lookup',
  )
where

import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
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
newtype PriceGraph priority cur = PriceGraph {unPriceGraph :: Map cur (Map cur (Direction (ConversionRate, priority)))}
  deriving (Show, Eq, Generic)

instance (Validity priority, Show priority, Ord priority, Validity cur, Show cur, Ord cur) => Validity (PriceGraph priority cur)

-- We remember which direction the rate was added in, for the graph command.
data Direction a = Forward a | Backward a
  deriving (Show, Eq, Generic)

instance (Validity a) => Validity (Direction a)

unDirection :: Direction a -> a
unDirection = \case
  Forward a -> a
  Backward a -> a

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
        M.insertWith M.union to (M.singleton from (Backward (ConversionRate.invert rate, priority))) $
          M.insertWith M.union from (M.singleton to (Forward (rate, priority))) m

fromList :: (Ord cur) => [((cur, cur), (Money.ConversionRate, priority))] -> PriceGraph priority cur
fromList = foldl' (\ps ((c1, c2), (r, p)) -> insert c1 c2 r p ps) empty

lookup ::
  forall priority cur.
  (Show priority, Ord priority, Ord cur) =>
  PriceGraph priority cur ->
  cur ->
  cur ->
  Maybe Money.ConversionRate
lookup pg from to =
  if from == to
    then Just ConversionRate.oneToOne
    else go ConversionRate.oneToOne . fst <$> lookup' pg from to
  where
    go r = \case
      PathStart _ -> r
      PathFrom _ r' p -> go (ConversionRate.compose r r') p

-- Strategy note: We use the path with the most recent prices, and of those the
-- shortest one.
--
-- Note that this function only returns paths with at least one step.
-- A -> A will not be considered here but in 'lookup'.
lookup' ::
  forall priority cur.
  (Show priority, Ord priority, Ord cur) =>
  PriceGraph priority cur ->
  cur ->
  cur ->
  Maybe (Path cur Money.ConversionRate, WorstPriorityCost priority)
lookup' (PriceGraph m) =
  dijkstra edgesFrom buildWorstPriorityCost (<>)
  where
    edgesFrom :: cur -> Map cur (Money.ConversionRate, priority)
    edgesFrom n = M.map unDirection $ fromMaybe M.empty $ M.lookup n m

-- Sort by minimal priority in the chain first (bigger is better, should come first), then the
-- length of the chain (shorter is better, should come first).
data WorstPriorityCost priority = WorstPriorityCost (Down priority) Word
  deriving (Show, Eq, Ord)

instance (Show priority, Ord priority) => Semigroup (WorstPriorityCost priority) where
  WorstPriorityCost (Down p1) l1 <> WorstPriorityCost (Down p2) l2 =
    -- Remember the worst (lowest) priority as the one to sort by.
    WorstPriorityCost (Down (min p1 p2)) (l1 + l2)

buildWorstPriorityCost :: priority -> WorstPriorityCost priority
buildWorstPriorityCost p = WorstPriorityCost (Down p) 1

dijkstra ::
  forall node edge priority cost.
  (Ord node, Ord cost) =>
  (node -> Map node (edge, priority)) ->
  (priority -> cost) ->
  (cost -> cost -> cost) ->
  node ->
  node ->
  -- Reverse path from goal to start
  Maybe (Path node edge, cost)
dijkstra
  getEdges
  buildCost
  combineCost
  start
  goal = do
    (path, mCost) <-
      go
        (M.singleton start (PathStart start, Nothing))
        (PSQ.singleton start Nothing ())
    cost <- mCost
    pure (path, cost)
    where
      go ::
        Map node (Path node edge, Maybe cost) ->
        OrdPSQ node (Maybe cost) () ->
        Maybe (Path node edge, Maybe cost)
      go visited queue = case PSQ.minView queue of
        -- If the queue is empty, the visited map now has the shortest path
        -- from any node to the start.
        Nothing -> M.lookup goal visited
        Just (currentNode, _, (), restQueue) ->
          let (newVisited, newQueue) =
                foldl'
                  (considerEdge currentNode)
                  (visited, restQueue)
                  (M.toList (getEdges currentNode))
           in go newVisited newQueue

      considerEdge ::
        node ->
        ( Map node (Path node edge, Maybe cost),
          OrdPSQ node (Maybe cost) ()
        ) ->
        (node, (edge, priority)) ->
        ( Map node (Path node edge, Maybe cost),
          OrdPSQ node (Maybe cost) ()
        )
      considerEdge currentNode (visited, queue) (node, (edge, priority)) =
        let ignoreThisEdge = (visited, queue)
         in case M.lookup currentNode visited of
              Nothing ->
                -- If the distance is infinite, don't consider this edge.
                ignoreThisEdge
              Just (pathSoFar, mCostSoFar) ->
                let pathToNode = PathFrom node edge pathSoFar
                    costToNode = case mCostSoFar of
                      Nothing -> buildCost priority
                      Just costSoFar -> combineCost costSoFar (buildCost priority)
                    newVisited = M.insert node (pathToNode, Just costToNode) visited
                    newQueue = PSQ.insert node (Just costToNode) () queue
                    foundNewBetterCost = (newVisited, newQueue)
                 in case M.lookup node visited of
                      Nothing ->
                        -- First path to this node, set the cost
                        foundNewBetterCost
                      Just (_, bestCostSoFar) ->
                        -- New path to this node, only set the cost if it's lower.
                        if Just costToNode < bestCostSoFar
                          then foundNewBetterCost
                          else ignoreThisEdge

-- Path from start to goal
data Path node edge
  = PathStart node
  | PathFrom node edge (Path node edge)
  deriving (Show)
