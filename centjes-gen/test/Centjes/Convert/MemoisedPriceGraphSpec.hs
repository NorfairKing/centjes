{-# LANGUAGE TypeApplications #-}

module Centjes.Convert.MemoisedPriceGraphSpec (spec) where

import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Convert.PriceGraph (PriceGraph)
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
      producesValid (MemoisedPriceGraph.fromPriceGraph @Int @CurrencySymbol)

  describe "lookup" $ do
    it "produces one for a currency and itself" $
      forAllValid $ \graph ->
        forAllValid $ \c ->
          MemoisedPriceGraph.lookup @CurrencySymbol (MemoisedPriceGraph.fromPriceGraph (graph :: PriceGraph Int CurrencySymbol)) c c
            `shouldBe` Just ConversionRate.oneToOne

    it "produces the same results as just using a price graph" $
      forAllValid $ \graph ->
        forAllValid $ \from ->
          forAll (genValid `suchThat` (/= from)) $ \to ->
            MemoisedPriceGraph.lookup @CurrencySymbol (MemoisedPriceGraph.fromPriceGraph graph) from to
              `shouldBe` PriceGraph.lookup @Int @CurrencySymbol graph from to
