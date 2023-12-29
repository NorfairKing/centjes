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

fromToInverted :: Ord a => a -> a -> Bool
fromToInverted = (>)

fromToRate :: Ord a => a -> a -> Money.ConversionRate -> Money.ConversionRate
fromToRate a1 a2 = fst $ mkFromTo' a1 a2

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

          insertAll ::
            [(FromTo cur, Hops cur)] ->
            Map (FromTo cur) (Hops cur) ->
            Map (FromTo cur) (Hops cur)
          insertAll tups m = foldl' (\m' (ft, h) -> M.insert ft h m') m tups

          -- Hops that arrive at the from'
          singleHopsArriving :: [(FromTo cur, Hops cur)]
          singleHopsArriving = do
            c <- allCurrencies
            let (arrivingF, arrivingFromTo) = mkFromTo' c from'
            arrivingRate <- maybeToList $ arrivingF . hopsRate <$> M.lookup arrivingFromTo m
            pure (arrivingFromTo, HopVia from' (ConversionRate.compose arrivingRate rate))
          -- Hops that arrive at the from'
          singleHopsDeparting :: [(FromTo cur, Hops cur)]
          singleHopsDeparting = do
            c <- allCurrencies
            let (departingF, departingFromTo) = mkFromTo' to' c
            departingRate <- maybeToList $ departingF . hopsRate <$> M.lookup departingFromTo m
            pure (departingFromTo, HopVia to' (ConversionRate.compose rate departingRate))
       in Prices $ insertAll singleHopsArriving $ insertAll singleHopsDeparting directInserted

fromList :: Ord cur => [((cur, cur), Money.ConversionRate)] -> Prices cur
fromList = foldl' (\ps ((c1, c2), r) -> insert c1 c2 r ps) empty

lookupConversionFactor :: Ord cur => Prices cur -> cur -> cur -> Maybe Money.ConversionRate
lookupConversionFactor (Prices m) from to
  | from == to = Just ConversionRate.oneToOne
  | otherwise = do
      let (f, fromTo) = mkFromTo' from to
      hops <- M.lookup fromTo m
      pure $ f $ hopsRate hops
