{-# LANGUAGE TypeApplications #-}

module Centjes.Convert.PriceGraphSpec (spec) where

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
  genValidSpec @(PriceGraph Int String)

  describe "empty" $
    it "is valid" $
      shouldBeValid (PriceGraph.empty @Int @CurrencySymbol)

  describe "singleton" $ do
    it "is valid" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \priority ->
              shouldBeValid
                (PriceGraph.singleton @Int @CurrencySymbol from to rate priority)

    it "is the same as inserting one element" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \priority ->
              PriceGraph.insert @Int @CurrencySymbol from to rate priority PriceGraph.empty `shouldBe` PriceGraph.singleton from to rate priority

  describe "insert" $
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \priority ->
              forAllValid $ \prices ->
                shouldBeValid $ PriceGraph.insert @Int @CurrencySymbol from to rate priority prices

  describe "fromList" $
    it "produces valid prices" $
      producesValid $
        PriceGraph.fromList @Int @CurrencySymbol

  describe "lookup" $ do
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \prices ->
            shouldBeValid $ PriceGraph.lookup @Int @CurrencySymbol prices from to

    it "produces one for a currency and itself" $
      forAllValid $ \prices ->
        forAllValid $ \c ->
          PriceGraph.lookup @Int @CurrencySymbol prices c c `shouldBe` Just ConversionRate.oneToOne

    it "cannot convert anything with empty prices" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          if from == to
            then pure ()
            else PriceGraph.lookup @Int @CurrencySymbol PriceGraph.empty from to `shouldBe` Nothing

    it "can find a single rate in one direction" $
      forAllValid $ \from ->
        forAll (genValid `suchThat` (/= from)) $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \priority ->
              PriceGraph.lookup @Int @CurrencySymbol (PriceGraph.singleton from to rate priority) from to
                `shouldBe` Just rate

    it "can find a single rate in the reverse direction" $
      forAllValid $ \from ->
        forAll (genValid `suchThat` (/= from)) $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \priority ->
              PriceGraph.lookup @Int @CurrencySymbol (PriceGraph.singleton from to rate priority) to from
                `shouldBe` Just (ConversionRate.invert rate)

    it "can a find a one-hop rate" $
      forAllValid $ \c1 ->
        forAll (genValid `suchThat` (/= c1)) $ \c2 ->
          forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2)) $ \c3 ->
            forAllValid $ \(r1, p1) ->
              forAllValid $ \(r2, p2) -> do
                let ps = PriceGraph.fromList [((c1, c2), (r1, p1)), ((c2, c3), (r2, p2))]
                context (ppShow ps) $
                  PriceGraph.lookup @Int @CurrencySymbol ps c1 c3 `shouldBe` Just (ConversionRate.compose r1 r2)

    it "can a find a zero-hop rate even if multiple hops are available" $
      forAllValid $ \c1 ->
        forAll (genValid `suchThat` (/= c1)) $ \c2 ->
          forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2)) $ \c3 ->
            forAllValid $ \(r1, p1) ->
              forAllValid $ \(r2, p2) ->
                forAllValid $ \(r3, p3) ->
                  let ps = PriceGraph.fromList [((c1, c2), (r1, p1)), ((c2, c3), (r2, p2)), ((c1, c3), (r3, p3))]
                   in PriceGraph.lookup @Int @CurrencySymbol ps c1 c3 `shouldBe` Just r3

    it "can a find a two-hop rate" $
      forAllValid $ \c1 ->
        forAll (genValid `suchThat` (/= c1)) $ \c2 ->
          forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2)) $ \c3 ->
            forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2 && c /= c3)) $ \c4 ->
              forAllValid $ \(r1, p1) ->
                forAllValid $ \(r2, p2) ->
                  forAllValid $ \(r3, p3) ->
                    let ps = PriceGraph.fromList [((c1, c2), (r1, p1)), ((c2, c3), (r2, p2)), ((c3, c4), (r3, p3))]
                     in PriceGraph.lookup @Int @CurrencySymbol ps c1 c4 `shouldBe` Just (ConversionRate.compose r1 (ConversionRate.compose r2 r3))
