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
  genValidSpec @(PriceGraph String)

  describe "empty" $
    it "is valid" $
      shouldBeValid (PriceGraph.empty @CurrencySymbol)

  describe "singleton" $ do
    it "is valid" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            shouldBeValid
              (PriceGraph.singleton @CurrencySymbol from to rate)

    it "is the same as inserting one element" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            PriceGraph.insert @CurrencySymbol from to rate PriceGraph.empty `shouldBe` PriceGraph.singleton from to rate

  describe "insert" $
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \prices ->
              shouldBeValid $ PriceGraph.insert @CurrencySymbol from to rate prices

  describe "fromList" $
    it "produces valid prices" $
      producesValid $
        PriceGraph.fromList @CurrencySymbol

  describe "lookup" $ do
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \prices ->
            shouldBeValid $ PriceGraph.lookup @CurrencySymbol prices from to

    it "produces one for a currency and itself" $
      forAllValid $ \prices ->
        forAllValid $ \c ->
          PriceGraph.lookup @CurrencySymbol prices c c `shouldBe` Just ConversionRate.oneToOne

    it "cannot convert anything with empty prices" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          if from == to
            then pure ()
            else PriceGraph.lookup @CurrencySymbol PriceGraph.empty from to `shouldBe` Nothing

    it "can find a single rate in one direction" $
      forAllValid $ \from ->
        forAll (genValid `suchThat` (/= from)) $ \to ->
          forAllValid $ \rate ->
            PriceGraph.lookup @CurrencySymbol (PriceGraph.singleton from to rate) from to
              `shouldBe` Just rate

    it "can find a single rate in the reverse direction" $
      forAllValid $ \from ->
        forAll (genValid `suchThat` (/= from)) $ \to ->
          forAllValid $ \rate ->
            PriceGraph.lookup @CurrencySymbol (PriceGraph.singleton from to rate) to from
              `shouldBe` Just (ConversionRate.invert rate)

    it "can a find a one-hop rate" $
      forAllValid $ \c1 ->
        forAll (genValid `suchThat` (/= c1)) $ \c2 ->
          forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2)) $ \c3 ->
            forAllValid $ \r1 ->
              forAllValid $ \r2 -> do
                let ps = PriceGraph.fromList [((c1, c2), r1), ((c2, c3), r2)]
                context (ppShow ps) $
                  PriceGraph.lookup @CurrencySymbol ps c1 c3 `shouldBe` Just (ConversionRate.compose r1 r2)

    it "can a find a zero-hop rate even if multiple hops are available" $
      forAllValid $ \c1 ->
        forAll (genValid `suchThat` (/= c1)) $ \c2 ->
          forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2)) $ \c3 ->
            forAllValid $ \r1 ->
              forAllValid $ \r2 ->
                forAllValid $ \r3 ->
                  let ps = PriceGraph.fromList [((c1, c2), r1), ((c2, c3), r2), ((c1, c3), r3)]
                   in PriceGraph.lookup @CurrencySymbol ps c1 c3 `shouldBe` Just r3

    it "can a find a two-hop rate" $
      forAllValid $ \c1 ->
        forAll (genValid `suchThat` (/= c1)) $ \c2 ->
          forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2)) $ \c3 ->
            forAll (genValid `suchThat` (\c -> c /= c1 && c /= c2 && c /= c3)) $ \c4 ->
              forAllValid $ \r1 ->
                forAllValid $ \r2 ->
                  forAllValid $ \r3 ->
                    let ps = PriceGraph.fromList [((c1, c2), r1), ((c2, c3), r2), ((c3, c4), r3)]
                     in PriceGraph.lookup @CurrencySymbol ps c1 c4 `shouldBe` Just (ConversionRate.compose r1 (ConversionRate.compose r2 r3))
