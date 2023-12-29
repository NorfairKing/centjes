{-# LANGUAGE TypeApplications #-}

module Centjes.Convert.MemoisedPriceGraphSpec (spec) where

import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph (..))
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Convert.PriceGraph (PriceGraph (..))
import qualified Centjes.Convert.PriceGraph as PriceGraph
import Centjes.Convert.PriceGraph.Gen ()
import Centjes.CurrencySymbol
import Centjes.CurrencySymbol.Gen ()
import qualified Money.ConversionRate as ConversionRate
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "fromPriceGraph" $
    it "is valid" $
      producesValid (MemoisedPriceGraph.fromPriceGraph @CurrencySymbol)

  describe "lookup" $
    it "produces the same results as just using a price graph" $
      forAllValid $ \graph ->
        forAllValid $ \from ->
          forAllValid $ \to ->
            MemoisedPriceGraph.lookup @CurrencySymbol (MemoisedPriceGraph.fromPriceGraph graph) from to
              `shouldBe` PriceGraph.lookup graph from to
