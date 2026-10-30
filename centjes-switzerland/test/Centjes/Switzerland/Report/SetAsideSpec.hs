module Centjes.Switzerland.Report.SetAsideSpec (spec) where

import Centjes.Switzerland.Report.SetAside
import Money.Account (Account (..))
import qualified Money.Amount as Amount
import Test.QuickCheck (choose, elements, forAll)
import Test.Syd

spec :: Spec
spec = do
  describe "shareOfRevenue" $ do
    it "takes a whole percentage" $
      shareOfRevenue 0.12 (Positive (Amount.fromMinimalQuantisations 1000000))
        `shouldBe` Just (Positive (Amount.fromMinimalQuantisations 120000))

    it "takes a percentage with decimals exactly" $
      shareOfRevenue 0.081 (Positive (Amount.fromMinimalQuantisations 1000000))
        `shouldBe` Just (Positive (Amount.fromMinimalQuantisations 81000))

    it "rounds to the nearest rappen" $
      -- 12% of 116.60 is 13.992
      shareOfRevenue 0.12 (Positive (Amount.fromMinimalQuantisations 11660))
        `shouldBe` Just (Positive (Amount.fromMinimalQuantisations 1399))

    it "is negative for negative revenue" $
      shareOfRevenue 0.02 (Negative (Amount.fromMinimalQuantisations 10000))
        `shouldBe` Just (Negative (Amount.fromMinimalQuantisations 200))

  describe "roundUpToHundreds" $ do
    it "rounds a shortfall up to the next hundred" $
      roundUpToHundreds (Amount.fromMinimalQuantisations 84220) `shouldBe` Just (Amount.fromMinimalQuantisations 90000)

    it "keeps a whole hundred" $
      roundUpToHundreds (Amount.fromMinimalQuantisations 90000) `shouldBe` Just (Amount.fromMinimalQuantisations 90000)

    it "rounds one rappen up to a hundred" $
      roundUpToHundreds (Amount.fromMinimalQuantisations 1) `shouldBe` Just (Amount.fromMinimalQuantisations 10000)

  describe "zurichCantonalSteuerfuss" $ do
    it "is 95% for 2026" $
      zurichCantonalSteuerfuss 2026 `shouldBe` Just 95

    it "does not know years before 2026" $
      zurichCantonalSteuerfuss 2025 `shouldBe` Nothing

  describe "zurichMunicipalSteuerfuss" $ do
    it "is 119% for the city of Zürich in 2026" $
      zurichMunicipalSteuerfuss (Municipality 261) 2026 `shouldBe` Just 119

    it "does not know other municipalities" $
      zurichMunicipalSteuerfuss (Municipality 1) 2026 `shouldBe` Nothing

  describe "zurichMarriedWealthTariff" $ do
    it "does not know years before 2026" $
      zurichMarriedWealthTariff 2025 `shouldBe` Nothing

    it "keeps the 2026 brackets for 2027, in the same Steuerfuss period" $
      zurichMarriedWealthTariff 2027 `shouldBe` zurichMarriedWealthTariff 2026

  describe "simpleWealthTax" $ do
    it "is zero below the tax-free amount in 2026" $
      fmap (`simpleWealthTax` 161000) (zurichMarriedWealthTariff 2026) `shouldBe` Just 0

    it "goes through four brackets in 2026" $
      -- 242'000 at 0.5 per mille, 402'000 at 1, and 322'000 at 1.5
      fmap (`simpleWealthTax` 1127000) (zurichMarriedWealthTariff 2026) `shouldBe` Just 1006

    it "does not round wealth down, so that it never underestimates" $
      -- 999 more at 1.5 per mille
      fmap (`simpleWealthTax` 1127999) (zurichMarriedWealthTariff 2026) `shouldBe` Just 1007.4985

    it "counts rappen" $
      fmap (`simpleWealthTax` 1127000.5) (zurichMarriedWealthTariff 2026) `shouldBe` Just 1006.00075

    it "applies the top rate above the brackets in 2026" $
      fmap (`simpleWealthTax` 4385000) (zurichMarriedWealthTariff 2026) `shouldBe` Just 8843.5

    it "never decreases when wealth increases" $
      forAll (elements [2026, 2027]) $ \year ->
        forAll (choose (0, 10000000)) $ \wealth ->
          forAll (choose (0, 1000000)) $ \extra ->
            fmap (`simpleWealthTax` toRational (wealth + extra :: Integer)) (zurichMarriedWealthTariff year)
              >= fmap (`simpleWealthTax` toRational wealth) (zurichMarriedWealthTariff year)
