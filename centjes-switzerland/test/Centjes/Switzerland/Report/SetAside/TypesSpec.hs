{-# LANGUAGE OverloadedStrings #-}

module Centjes.Switzerland.Report.SetAside.TypesSpec (spec) where

import Centjes.Switzerland.Report.SetAside.Types
import qualified Data.Map as M
import Test.Syd

spec :: Spec
spec = do
  describe "duplicateSetAsideAccounts" $ do
    it "finds nothing when every reserve account is mentioned once" $
      duplicateSetAsideAccounts
        SetAsideInput
          { setAsideInputYear = 2026,
            setAsideInputTagUndeclared = "undeclared",
            setAsideInputSocialSecurity =
              SetAsideTax
                { setAsideTaxRate = 0.12,
                  setAsideTaxReserveAccounts = ["assets:sva"]
                },
            setAsideInputCantonal =
              SetAsideTax
                { setAsideTaxRate = 0.08,
                  setAsideTaxReserveAccounts = ["expenses:cantonal"]
                },
            setAsideInputFederal =
              SetAsideTax
                { setAsideTaxRate = 0.02,
                  setAsideTaxReserveAccounts = ["assets:federal"]
                },
            setAsideInputWealth =
              SetAsideWealth
                { setAsideWealthMunicipality = Municipality 261,
                  setAsideWealthTagTaxable = "taxable",
                  setAsideWealthTagNotTaxable = "not-taxable",
                  setAsideWealthOwnedShares = M.singleton "assets:joint" 0.53,
                  setAsideWealthReserveAccounts = ["assets:wealth-tax"]
                }
          }
        `shouldBe` []

    it "finds a reserve account of two taxes" $
      duplicateSetAsideAccounts
        SetAsideInput
          { setAsideInputYear = 2026,
            setAsideInputTagUndeclared = "undeclared",
            setAsideInputSocialSecurity =
              SetAsideTax
                { setAsideTaxRate = 0.12,
                  setAsideTaxReserveAccounts = ["assets:taxes"]
                },
            setAsideInputCantonal =
              SetAsideTax
                { setAsideTaxRate = 0.08,
                  setAsideTaxReserveAccounts = ["expenses:cantonal"]
                },
            setAsideInputFederal =
              SetAsideTax
                { setAsideTaxRate = 0.02,
                  setAsideTaxReserveAccounts = []
                },
            setAsideInputWealth =
              SetAsideWealth
                { setAsideWealthMunicipality = Municipality 261,
                  setAsideWealthTagTaxable = "taxable",
                  setAsideWealthTagNotTaxable = "not-taxable",
                  setAsideWealthOwnedShares = M.singleton "assets:joint" 0.53,
                  setAsideWealthReserveAccounts = ["assets:taxes"]
                }
          }
        `shouldBe` ["assets:taxes"]

    it "finds a reserve account mentioned twice for one tax" $
      duplicateSetAsideAccounts
        SetAsideInput
          { setAsideInputYear = 2026,
            setAsideInputTagUndeclared = "undeclared",
            setAsideInputSocialSecurity =
              SetAsideTax
                { setAsideTaxRate = 0.12,
                  setAsideTaxReserveAccounts = ["assets:sva", "assets:sva"]
                },
            setAsideInputCantonal =
              SetAsideTax
                { setAsideTaxRate = 0.08,
                  setAsideTaxReserveAccounts = []
                },
            setAsideInputFederal =
              SetAsideTax
                { setAsideTaxRate = 0.02,
                  setAsideTaxReserveAccounts = []
                },
            setAsideInputWealth =
              SetAsideWealth
                { setAsideWealthMunicipality = Municipality 261,
                  setAsideWealthTagTaxable = "taxable",
                  setAsideWealthTagNotTaxable = "not-taxable",
                  setAsideWealthOwnedShares = M.singleton "assets:joint" 0.53,
                  setAsideWealthReserveAccounts = []
                }
          }
        `shouldBe` ["assets:sva"]

  describe "taxability" $ do
    it "counts an account tagged taxable" $
      taxability
        SetAsideWealth
          { setAsideWealthMunicipality = Municipality 261,
            setAsideWealthTagTaxable = "taxable",
            setAsideWealthTagNotTaxable = "not-taxable",
            setAsideWealthOwnedShares = M.empty,
            setAsideWealthReserveAccounts = []
          }
        (M.singleton "taxable" (1 :: Int))
        `shouldBe` Taxable

    it "leaves out an account tagged not-taxable" $
      taxability
        SetAsideWealth
          { setAsideWealthMunicipality = Municipality 261,
            setAsideWealthTagTaxable = "taxable",
            setAsideWealthTagNotTaxable = "not-taxable",
            setAsideWealthOwnedShares = M.empty,
            setAsideWealthReserveAccounts = []
          }
        (M.singleton "not-taxable" (1 :: Int))
        `shouldBe` NotTaxable

    it "ignores other tags" $
      taxability
        SetAsideWealth
          { setAsideWealthMunicipality = Municipality 261,
            setAsideWealthTagTaxable = "taxable",
            setAsideWealthTagNotTaxable = "not-taxable",
            setAsideWealthOwnedShares = M.empty,
            setAsideWealthReserveAccounts = []
          }
        (M.fromList [("undeclared", 1 :: Int), ("taxable", 2)])
        `shouldBe` Taxable

    it "finds an untagged account" $
      taxability
        SetAsideWealth
          { setAsideWealthMunicipality = Municipality 261,
            setAsideWealthTagTaxable = "taxable",
            setAsideWealthTagNotTaxable = "not-taxable",
            setAsideWealthOwnedShares = M.empty,
            setAsideWealthReserveAccounts = []
          }
        (M.singleton "undeclared" (1 :: Int))
        `shouldBe` Untagged

    it "finds an account tagged both" $
      taxability
        SetAsideWealth
          { setAsideWealthMunicipality = Municipality 261,
            setAsideWealthTagTaxable = "taxable",
            setAsideWealthTagNotTaxable = "not-taxable",
            setAsideWealthOwnedShares = M.empty,
            setAsideWealthReserveAccounts = []
          }
        (M.fromList [("taxable", 1 :: Int), ("not-taxable", 2)])
        `shouldBe` TaggedBoth 1 2

  describe "parseOwnedShares" $ do
    it "accepts nothing and everything" $
      parseOwnedShares (M.fromList [("assets:a", 0), ("assets:b", 1)])
        `shouldBe` Right (M.fromList [("assets:a", 0), ("assets:b", 1)])

    it "rejects an invalid account name" $
      parseOwnedShares (M.singleton "assets::a" 0.5)
        `shouldBe` Left "Invalid account name: \"assets::a\""

    it "rejects a share above 1" $
      parseOwnedShares (M.singleton "assets:a" 53)
        `shouldBe` Left "Not between 0 and 1: 53.0"

  describe "parseFraction" $ do
    it "accepts a fraction" $
      parseFraction 0.12 `shouldBe` Right 0.12

    it "accepts 0 and 1" $
      (parseFraction 0, parseFraction 1) `shouldBe` (Right 0, Right 1)

    it "rejects a percentage written as a whole number" $
      parseFraction 12 `shouldBe` Left "Not between 0 and 1: 12.0"

    it "rejects a negative fraction" $
      parseFraction (-0.1) `shouldBe` Left "Not between 0 and 1: -0.1"
