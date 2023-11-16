{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.CompileSpec (spec) where

import Centjes.Compile
import qualified Centjes.Module as Module
import Centjes.Module.Gen ()
import Centjes.Validation.TestUtils
import Data.GenValidity.Map ()
import qualified Data.Map.Strict as M
import Test.QuickCheck (forAll)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "compileDeclarations" $ do
    it "produces valid ledgers" $
      producesValid compileDeclarations

  describe "compileTransaction" $ do
    it "produces valid ledgers" $
      producesValid2 compileTransaction

    it "produces valid ledger transactions if all the currencies are known" $
      forAllValid $ \transaction ->
        forAll (sequence (M.fromSet (const genValid) (Module.transactionCurrencySymbols transaction))) $ \currencies -> do
          t <- shouldValidate $ compileTransaction currencies transaction
          shouldBeValid t

  describe "compilePosting" $ do
    it "produces valid ledgers" $
      producesValid2 compilePosting

    it "produces valid ledger postings if the currency is known" $
      forAllValid $ \posting ->
        forAllValid $ \factor -> do
          p <- shouldValidate $ compilePosting (M.singleton (Module.postingCurrencySymbol posting) factor) posting
          shouldBeValid p
