{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert.MemoisedPriceGraph
  ( MemoisedPriceGraph (..),
    empty,
    fromPriceGraph,
    lookup,
  )
where

import Centjes.Convert.PriceGraph (PriceGraph (..))
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import Prelude hiding (lookup)

-- | A memoised price graph:
--
-- We use a lazy map keyed by target currency. Each value is a map from source
-- currency to conversion rate, computed by a single Dijkstra run from the
-- target. This way, converting multiple source currencies to the same target
-- only runs Dijkstra once.
newtype MemoisedPriceGraph cur = MemoisedPriceGraph {unMemoisedPriceGraph :: Map cur (Map cur ConversionRate)}
  deriving (Show, Generic)

instance (Validity cur, Show cur, Ord cur) => Validity (MemoisedPriceGraph cur)

empty :: MemoisedPriceGraph cur
empty = MemoisedPriceGraph M.empty

fromPriceGraph :: (Ord cur, Show priority, Ord priority) => PriceGraph priority cur -> MemoisedPriceGraph cur
fromPriceGraph pg@(PriceGraph m) =
  let allCurrencies = S.fromList $ do
        (from, m') <- M.toList m
        from : M.keys m'
      allTargets = M.fromList $ do
        to <- S.toList allCurrencies
        pure (to, PriceGraph.lookupAllTo pg to)
   in MemoisedPriceGraph allTargets

lookup :: forall cur. (Ord cur) => MemoisedPriceGraph cur -> cur -> cur -> Maybe Money.ConversionRate
lookup (MemoisedPriceGraph m) from to =
  if from == to
    then Just ConversionRate.oneToOne
    else M.lookup to m >>= M.lookup from
