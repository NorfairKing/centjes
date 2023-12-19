{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Command.CheckSpec (spec) where

import Centjes.Command.Check
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Module.Gen ()
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import Data.GenValidity.Containers ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "checkAccountsUnique" $ do
    it "produces valid errors" $
      producesValid (checkAccountsUnique @())
    it "finds duplicate accounts" $
      forAllValid $ \an -> do
        let ad1 = AccountDeclaration {accountDeclarationName = noLoc an}
        let ad2 = AccountDeclaration {accountDeclarationName = noLoc an}
        errs <- shouldFailToValidate $ checkAccountsUnique [DeclarationAccount $ noLoc ad1, DeclarationAccount $ noLoc ad2]
        errs `shouldBe` CheckErrorAccountDeclaredTwice () () an :| []

  describe "checkAccountsDeclared" $ do
    it "produces valid errors" $
      producesValid2 (checkAccountsDeclared @())
    it "finds undeclared accounts" $
      forAllValid $ \t' ->
        forAllValid $ \p -> do
          let t :: GenLocated () (Transaction ())
              t = noLoc $ t' {transactionPostings = [noLoc p]}
              Located _ an = postingAccountName p
          let td = DeclarationTransaction t
          errs <- shouldFailToValidate $ checkAccountsDeclared M.empty [td]
          errs `shouldBe` CheckErrorUndeclaredAccount () () (Located () an) :| []

  describe "doCompleteCheck" $ do
    scenarioDir "test_resources/check" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when checking this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Try to check
            errs <- shouldFailToValidateT $ doCompleteCheck ds
            pure $ renderValidationErrors diag errs
