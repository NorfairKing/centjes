{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Validity
import GHC.Generics (Generic)
import Money.ConversionRate as ConversionRate
import qualified Money.ConversionRate as Money (ConversionRate)
import qualified Money.QuantisationFactor as Money (QuantisationFactor)

newtype Prices cur = Prices {unPrices :: Map (FromTo cur) Money.ConversionRate}
  deriving (Show, Eq, Generic)

instance (Validity cur, Show cur, Ord cur) => Validity (Prices cur)

-- Strict, ordered pair
data FromTo cur = FromTo !cur !cur
  deriving (Show, Eq, Ord, Generic)

instance Validity cur => Validity (FromTo cur)

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

class IsCurrency cur where
  getQuantisationFactor :: cur -> Money.QuantisationFactor

instance IsCurrency (Currency ann) where
  getQuantisationFactor = locatedValue . currencyQuantisationFactor

empty :: Prices cur
empty = Prices M.empty

singleton :: Ord cur => cur -> cur -> Money.ConversionRate -> Prices cur
singleton from to rate =
  let (f, fromTo) = mkFromTo' from to
   in Prices $ M.singleton fromTo $ f rate

insert :: Ord cur => cur -> cur -> Money.ConversionRate -> Prices cur -> Prices cur
insert from to rate (Prices m) =
  let (f, fromTo) = mkFromTo' from to
   in Prices $ M.insert fromTo (f rate) m

fromList :: Ord cur => [((cur, cur), Money.ConversionRate)] -> Prices cur
fromList = foldl' (\ps ((c1, c2), r) -> insert c1 c2 r ps) empty

lookupConversionFactor :: Ord cur => Prices cur -> cur -> cur -> Maybe Money.ConversionRate
lookupConversionFactor (Prices m) from to
  | from == to = Just ConversionRate.oneToOne
  | otherwise =
      let (f, fromTo) = mkFromTo' from to
       in f <$> M.lookup fromTo m
