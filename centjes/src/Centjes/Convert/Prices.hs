{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Convert.Prices
  ( Prices (..),
    FromTo (..),
    Hops,
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
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import GHC.Generics (Generic)
import Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.QuantisationFactor as Money (QuantisationFactor)

newtype Prices cur = Prices {unPrices :: Map (FromTo cur) (Hops cur)}
  deriving (Show, Eq, Generic)

instance (Validity cur, Show cur, Ord cur) => Validity (Prices cur)

-- Strict, ordered pair
data FromTo cur = FromTo !cur !cur
  deriving (Show, Eq, Ord, Generic)

instance Validity cur => Validity (FromTo cur)

data Hops cur
  = -- This FromTo has a direct rate
    HopFinal !Money.ConversionRate
  | -- This FromTo has an indirect rate.
    -- r_AC is composed from r_AB and r_BC
    HopVia !cur !Money.ConversionRate
  deriving (Show, Eq, Generic)

instance Validity cur => Validity (Hops cur)

hopsRate :: Hops cur -> Money.ConversionRate
hopsRate = \case
  HopFinal r -> r
  HopVia _ r -> r

hopsShorter :: Hops cur -> Hops cur -> Bool
hopsShorter h1 h2 = case (h1, h2) of
  (HopFinal _, HopFinal _) -> False
  (HopFinal _, HopVia _ _) -> True
  (HopVia _ _, _) -> False

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
       in Prices $ M.singleton fromTo (HopFinal (f rate))

-- TODO precompute this?
currencies :: Ord cur => Prices cur -> Set cur
currencies (Prices m) = S.unions $ map fromToSet $ M.keys m

insert :: forall cur. Ord cur => cur -> cur -> Money.ConversionRate -> Prices cur -> Prices cur
insert from to rate p@(Prices m) =
  if from == to
    then p
    else
      let (f, fromTo@(FromTo from' to')) = mkFromTo' from to
          directInserted = M.insert fromTo (HopFinal (f rate)) m
          allCurrencies = S.toList $ currencies p

          insertShorter ::
            forall cur.
            Ord cur =>
            FromTo cur ->
            Hops cur ->
            Map (FromTo cur) (Hops cur) ->
            Map (FromTo cur) (Hops cur)
          insertShorter fromTo hops m = case M.lookup fromTo m of
            Nothing -> M.insert fromTo hops m
            Just alreadyThere ->
              if alreadyThere `hopsShorter` hops
                then m
                else M.insert fromTo hops m

          insertAll ::
            [(FromTo cur, Hops cur)] ->
            Map (FromTo cur) (Hops cur) ->
            Map (FromTo cur) (Hops cur)
          insertAll tups m = foldl' (\m' (ft, h) -> insertShorter ft h m') m tups

          -- If we have AB and add BC, then we also want to add AC via B
          singleHops :: [(FromTo cur, Hops cur)]
          singleHops = do
            -- For every currency that we already know about: ['A', 'B']
            c <- allCurrencies
            -- That isn't one of the ones we are adding now, so not ['B', 'C']
            guard $ c /= from' && c /= to'
            -- For each of the ones in the pair, and the other: [('B', 'C'), ('C', 'B')]
            (via, other) <- [(from', to'), (to', from')]
            -- Make the connecting fromTo: [FromTo 'A' 'B', FromTo 'A' 'C']
            -- to look up the rate of that connector
            let (connectingF, connectingFromTo) = mkFromTo' c via
            let (hoppingF, hoppingFromTo) = mkFromTo' c other
            connectingRate <- maybeToList $ connectingF . hopsRate <$> M.lookup connectingFromTo m
            pure (hoppingFromTo, HopVia via $ hoppingF $ ConversionRate.compose connectingRate rate)
       in Prices $ insertAll singleHops directInserted

fromList :: Ord cur => [((cur, cur), Money.ConversionRate)] -> Prices cur
fromList = foldl' (\ps ((c1, c2), r) -> insert c1 c2 r ps) empty

lookupConversionFactor :: Ord cur => Prices cur -> cur -> cur -> Maybe Money.ConversionRate
lookupConversionFactor (Prices m) from to
  | from == to = Just ConversionRate.oneToOne
  | otherwise = do
      let (f, fromTo) = mkFromTo' from to
      hops <- M.lookup fromTo m
      pure $ f $ hopsRate hops
