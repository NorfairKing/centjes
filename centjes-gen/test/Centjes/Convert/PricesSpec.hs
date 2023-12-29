{-# LANGUAGE TypeApplications #-}

module Centjes.Convert.PricesSpec (spec) where

import Centjes.Convert.Prices (Prices (..))
import qualified Centjes.Convert.Prices as Prices
import Centjes.Convert.Prices.Gen ()
import Centjes.Ledger (Currency)
import Centjes.Ledger.Gen ()
import qualified Money.ConversionRate as ConversionRate
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(Prices (Currency ()))

  describe "empty" $
    it "is valid" $
      shouldBeValid (Prices.empty @(Currency ()))

  describe "singleton" $ do
    it "is valid" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            shouldBeValid
              (Prices.singleton @(Currency ()) from to rate)
    it "is the same as inserting one element" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            Prices.insert @(Currency ()) from to rate Prices.empty `shouldBe` Prices.singleton from to rate

  describe "insert" $
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            forAllValid $ \prices ->
              shouldBeValid $ Prices.insert @(Currency ()) from to rate prices

  describe "fromList" $
    it "produces valid prices" $
      producesValid $
        Prices.fromList @(Currency ())

  describe "lookupConversionFactor" $ do
    it "produces valid values" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \prices ->
            shouldBeValid $ Prices.lookupConversionFactor @(Currency ()) prices from to

    it "produces one for a currency and itself" $
      forAllValid $ \prices ->
        forAllValid $ \c ->
          Prices.lookupConversionFactor @(Currency ()) prices c c `shouldBe` Just ConversionRate.oneToOne

    it "cannot convert anything with empty prices" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          if from == to
            then pure ()
            else Prices.lookupConversionFactor @(Currency ()) Prices.empty from to `shouldBe` Nothing

    it "can find a single rate in one direction" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            Prices.lookupConversionFactor @(Currency ()) (Prices.singleton from to rate) from to
              `shouldBe` Just rate

    it "can find a single rate in the reverse direction" $
      forAllValid $ \from ->
        forAllValid $ \to ->
          forAllValid $ \rate ->
            Prices.lookupConversionFactor @(Currency ()) (Prices.singleton from to rate) to from
              `shouldBe` Just (ConversionRate.invert rate)

    it "can a find a one-hop rate" $
      forAllValid $ \c1 ->
        forAllValid $ \c2 ->
          forAllValid $ \c3 ->
            forAllValid $ \r1 ->
              forAllValid $ \r2 ->
                let ps = Prices.fromList [((c1, c2), r1), ((c2, c3), r2)]
                 in Prices.lookupConversionFactor @(Currency ()) ps c1 c3 `shouldBe` Just (ConversionRate.compose r1 r2)
