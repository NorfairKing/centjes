module Centjes.Switzerland.Report.SetAsideSpec (spec) where

import Centjes.Switzerland.Report.SetAside
import Money.Account (Account (..))
import qualified Money.Account as Account
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

  describe "thirdPillarMaximum" $ do
    it "is 36'288 CHF for 2025" $
      thirdPillarMaximum 2025 `shouldBe` Just 36288

    it "is 36'288 CHF for 2026" $
      thirdPillarMaximum 2026 `shouldBe` Just 36288

    it "is 36'864 CHF for 2027" $
      thirdPillarMaximum 2027 `shouldBe` Just 36864

    it "does not know years before 2025" $
      thirdPillarMaximum 2024 `shouldBe` Nothing

  describe "thirdPillarLimit" $ do
    it "is a fifth of the net earned income" $
      thirdPillarLimit 36288 (Positive (Amount.fromMinimalQuantisations 7893700))
        `shouldBe` Just (ThirdPillarLimitShare 0.2 (Positive (Amount.fromMinimalQuantisations 1578740)))

    it "is the maximum when a fifth would be more" $
      thirdPillarLimit 36288 (Positive (Amount.fromMinimalQuantisations 20000000))
        `shouldBe` Just (ThirdPillarLimitMaximum (Positive (Amount.fromMinimalQuantisations 3628800)))

    it "is a fifth when that is exactly the maximum" $
      thirdPillarLimit 36288 (Positive (Amount.fromMinimalQuantisations 18144000))
        `shouldBe` Just (ThirdPillarLimitShare 0.2 (Positive (Amount.fromMinimalQuantisations 3628800)))

    it "is zero for a loss" $
      thirdPillarLimit 36288 (Negative (Amount.fromMinimalQuantisations 100000))
        `shouldBe` Just (ThirdPillarLimitShare 0.2 (Positive Amount.zero))

    it "never exceeds the maximum and never is negative" $
      forAll (choose (-100000000, 100000000)) $ \rappen ->
        case Account.fromMinimalQuantisations rappen >>= thirdPillarLimit 36288 of
          Nothing -> expectationFailure "should not overflow"
          Just limit ->
            Account.toMinimalQuantisations (thirdPillarLimitAmount limit)
              `shouldSatisfy` (\amount -> amount >= 0 && amount <= 3628800)

  describe "roundDownToHundreds" $ do
    it "rounds room down to the hundred below" $
      roundDownToHundreds (Amount.fromMinimalQuantisations 1578740) `shouldBe` Amount.fromMinimalQuantisations 1570000

    it "keeps a whole hundred" $
      roundDownToHundreds (Amount.fromMinimalQuantisations 90000) `shouldBe` Amount.fromMinimalQuantisations 90000

    it "rounds less than a hundred down to zero" $
      roundDownToHundreds (Amount.fromMinimalQuantisations 9999) `shouldBe` Amount.zero

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
