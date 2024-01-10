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
import Data.Sequence (ViewL (..), (|>))
import qualified Data.Sequence as Seq
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
newtype PriceGraph cur = PriceGraph {unPriceGraph :: Map cur (Map cur ConversionRate)}
  deriving (Show, Eq, Generic)

instance (Validity cur, Show cur, Ord cur) => Validity (PriceGraph cur)

empty :: PriceGraph cur
empty = PriceGraph M.empty

singleton :: Ord cur => cur -> cur -> Money.ConversionRate -> PriceGraph cur
singleton from to rate =
  if from == to
    then empty
    else insert from to rate empty

insert :: forall cur. Ord cur => cur -> cur -> Money.ConversionRate -> PriceGraph cur -> PriceGraph cur
insert from to rate p@(PriceGraph m) =
  if from == to
    then p
    else
      PriceGraph $
        M.insertWith M.union to (M.singleton from (ConversionRate.invert rate)) $
          M.insertWith M.union from (M.singleton to rate) m

fromList :: Ord cur => [((cur, cur), Money.ConversionRate)] -> PriceGraph cur
fromList = foldl' (\ps ((c1, c2), r) -> insert c1 c2 r ps) empty

lookup :: forall cur. Ord cur => PriceGraph cur -> cur -> cur -> Maybe Money.ConversionRate
lookup (PriceGraph m) from to =
  go ConversionRate.oneToOne
    <$> breadthFirstSearch edgesFrom from to
  where
    go r = \case
      PathStart _ -> r
      PathFrom _ r' p -> go (ConversionRate.compose r r') p
    edgesFrom :: cur -> Map cur Money.ConversionRate
    edgesFrom n = fromMaybe M.empty $ M.lookup n m

breadthFirstSearch ::
  Ord node =>
  (node -> Map node edge) ->
  node ->
  node ->
  -- Reverse path from goal to start
  Maybe (Path node edge)
breadthFirstSearch getEdges start goal =
  go (S.singleton start) (Seq.singleton (PathStart start))
  where
    go visited queue = case Seq.viewl queue of
      -- If the queue is empty, there's no path from the goal to the start
      EmptyL -> Nothing
      (currentPath :< restQueue) ->
        -- If the head is the goal node, this is path to the start and we're
        -- done.
        let currentNode = pathHead currentPath
         in if currentNode == goal
              then Just currentPath
              else
                let considerEdge (v, q) (node, edge) =
                      -- If this node is already explored, don't consider this edge
                      if S.member node visited
                        then (v, q)
                        else do
                          -- Consider this node explored now and
                          -- make a path and add it to the queue
                          let p = PathFrom node edge currentPath
                          (S.insert node v, q |> p)

                    (newVisited, newQueue) = foldl' considerEdge (visited, restQueue) (M.toList (getEdges currentNode))
                 in go newVisited newQueue

-- Path from start to goal
data Path node edge
  = PathStart node
  | PathFrom node edge (Path node edge)

-- | The tip of the path
pathHead :: Path node edge -> node
pathHead = \case
  PathStart cur -> cur
  PathFrom cur _ _ -> cur
