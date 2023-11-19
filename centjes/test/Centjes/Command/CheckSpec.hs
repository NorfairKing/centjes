{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.CheckSpec (spec) where

import Centjes.Command.Check
import Centjes.Module
import Centjes.Module.Gen ()
import Centjes.Validation.TestUtils
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as S
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "checkDeclarations" $
    it "produces valid errors" $
      producesValid checkDeclarations

  describe "checkAccountsUnique" $
    it "finds duplicate accounts" $
      forAllValid $ \an -> do
        let ad1 = AccountDeclaration {accountDeclarationName = an}
        let ad2 = AccountDeclaration {accountDeclarationName = an}
        errs <- shouldFail $ checkAccountsUnique [DeclarationAccount ad1, DeclarationAccount ad2]
        errs `shouldBe` CheckErrorAccountDeclaredTwice an :| []

  describe "checkAccountsDeclared" $
    it "finds undeclared accounts" $
      forAllValid $ \t' ->
        forAllValid $ \p -> do
          let t = t' {transactionPostings = [p]}
          let td = DeclarationTransaction t
          errs <- shouldFail $ checkAccountsDeclared S.empty [td]
          errs `shouldBe` CheckErrorUndeclaredAccount (postingAccountName p) :| []
