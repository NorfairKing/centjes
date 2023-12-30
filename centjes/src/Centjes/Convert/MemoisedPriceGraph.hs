{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert.MemoisedPriceGraph
  ( MemoisedPriceGraph (..),
    fromPriceGraph,
    lookup,
  )
where

import Centjes.Convert.PriceGraph (PriceGraph (..))
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Validity
import Data.Validity.Containers ()
import GHC.Generics (Generic)
import Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import Prelude hiding (lookup)

-- | A memoised price graph:
--
-- We use a lazy map, and store a lookup in a price graph for each tuple in the
-- original price graph.
-- The lookup will only be done the first time, after that laziness will
-- memoise the result for us.
newtype MemoisedPriceGraph cur = MemoisedPriceGraph {unMemoisedPriceGraph :: Map (cur, cur) ConversionRate}
  deriving (Show, Eq, Generic)

instance (Validity cur, Show cur, Ord cur) => Validity (MemoisedPriceGraph cur)

-- TODO: this could be even faster by using the already-computed memoised price
-- graph when looking up paths, but that would give up laziness in constructing
-- this graph.
fromPriceGraph :: Ord cur => PriceGraph cur -> MemoisedPriceGraph cur
fromPriceGraph pg@(PriceGraph m) =
  let allRates = do
        (from, tos) <- M.toList m
        (to, _) <- M.toList tos
        rate <- maybeToList $ PriceGraph.lookup pg from to
        pure ((from, to), rate)
   in MemoisedPriceGraph $ M.fromList allRates

lookup :: forall cur. Ord cur => MemoisedPriceGraph cur -> cur -> cur -> Maybe Money.ConversionRate
lookup (MemoisedPriceGraph m) from to = M.lookup (from, to) m
