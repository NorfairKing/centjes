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
