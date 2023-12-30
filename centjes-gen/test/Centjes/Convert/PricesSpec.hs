{-# LANGUAGE TypeApplications #-}

module Centjes.Convert.PricesSpec (spec) where

import Centjes.Convert.Prices (Prices (..))
import qualified Centjes.Convert.Prices as Prices
import Centjes.Convert.Prices.Gen ()
import Centjes.CurrencySymbol
import Centjes.CurrencySymbol.Gen ()
import qualified Money.ConversionRate as ConversionRate
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(Prices String)

  describe "empty" $
    it "is valid" $
      shouldBeValid (Prices.empty @CurrencySymbol)

  describe "singleton" $ do
    it "is valid" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            shouldBeValid
              (Prices.singleton @CurrencySymbol from to rate)
    it "is the same as inserting one element" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            Prices.insert @CurrencySymbol from to rate Prices.empty `shouldBe` Prices.singleton from to rate

  describe "insert" $
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \prices ->
              shouldBeValid $ Prices.insert @CurrencySymbol from to rate prices

  describe "fromList" $
    it "produces valid prices" $
      producesValid $
        Prices.fromList @CurrencySymbol

  describe "lookupConversionFactor" $ do
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \prices ->
            shouldBeValid $ Prices.lookupConversionFactor @CurrencySymbol prices from to

    it "produces one for a currency and itself" $
      forAllValid $ \prices ->
        forAllValid $ \c ->
          Prices.lookupConversionFactor @CurrencySymbol prices c c `shouldBe` Just ConversionRate.oneToOne

    it "cannot convert anything with empty prices" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          if from == to
            then pure ()
            else Prices.lookupConversionFactor @CurrencySymbol Prices.empty from to `shouldBe` Nothing

    it "can find a single rate in one direction" $
      forAllValid $ \from ->
        forAll (genValid `suchThat` (/= from)) $ \to ->
          forAllValid $ \rate ->
            Prices.lookupConversionFactor @CurrencySymbol (Prices.singleton from to rate) from to
              `shouldBe` Just rate

    it "can find a single rate in the reverse direction" $
      forAllValid $ \from ->
        forAll (genValid `suchThat` (/= from)) $ \to ->
          forAllValid $ \rate ->
            Prices.lookupConversionFactor @CurrencySymbol (Prices.singleton from to rate) to from
              `shouldBe` Just (ConversionRate.invert rate)

    it "can a find a one-hop rate" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 ->
          forAllValid $ \c3 ->
            forAllValid $ \r1 ->
              forAllValid $ \r2 -> do
                let ps = Prices.fromList [((c1, c2), r1), ((c2, c3), r2)]
                context (ppShow ps) $
                  Prices.lookupConversionFactor @CurrencySymbol ps c1 c3 `shouldBe` Just (ConversionRate.compose r1 r2)

    it "can a find a zero-hop rate even if multiple hops are available" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 ->
          forAllValid $ \c3 ->
            forAllValid $ \r1 ->
              forAllValid $ \r2 ->
                forAllValid $ \r3 ->
                  let ps = Prices.fromList [((c1, c2), r1), ((c2, c3), r2), ((c1, c3), r3)]
                   in Prices.lookupConversionFactor @CurrencySymbol ps c1 c3 `shouldBe` Just r3

    it "can a find a two-hop rate" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 ->
          forAllValid $ \c3 ->
            forAllValid $ \c4 ->
              forAllValid $ \r1 ->
                forAllValid $ \r2 ->
                  forAllValid $ \r3 ->
                    let ps = Prices.fromList [((c1, c2), r1), ((c2, c3), r2), ((c3, c4), r3)]
                     in Prices.lookupConversionFactor @CurrencySymbol ps c1 c4 `shouldBe` Just (ConversionRate.compose r1 (ConversionRate.compose r2 r3))
