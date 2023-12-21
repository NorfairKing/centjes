{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.CompileSpec (spec) where

import Centjes.Compile
import Centjes.Load
import Centjes.Location
import qualified Centjes.Module as Module
import Centjes.Module.Gen ()
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import Data.GenValidity.Map ()
import qualified Data.Map.Strict as M
import Path
import Path.IO
import Test.QuickCheck (forAll)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "compilePosting" $ do
    it "produces valid ledgers" $
      producesValid3 (compilePosting @())

    it "produces valid ledger postings if the currency is known" $
      forAllValid $ \posting ->
        forAllValid $ \factor ->
          let Located () symbol = Module.postingCurrencySymbol posting
           in case compilePosting (M.singleton symbol factor) () (Located () posting) of
                Failure _ -> pure ()
                Success p -> shouldBeValid p

  describe "compileTransaction" $ do
    it "produces valid ledgers" $
      producesValid2 (compileTransaction @())

    it "produces valid ledger transactions if all the currencies are known" $
      forAllValid $ \transaction ->
        forAll (sequence (M.fromSet (const genValid) (Module.transactionCurrencySymbols transaction))) $ \currencies -> do
          case compileTransaction currencies (Located () transaction) of
            Failure _ -> pure ()
            Success t -> shouldBeValid t

  describe "compileCurrencyDeclaration" $ do
    it "produces valid ledgers" $
      producesValid (compileCurrencyDeclaration @())

  describe "compileCurrencyDeclarationDeclarations" $ do
    it "produces valid ledgers" $
      producesValid (compileCurrencyDeclarations @())

  describe "compileDeclarations" $ do
    it "produces valid ledgers" $
      producesValid (compileDeclarations @())

  scenarioDir "test_resources/compile" $ \fp -> do
    af <- liftIO $ resolveFile' fp
    when (fileExtension af == Just ".cent") $ do
      resultFile <- liftIO $ replaceExtension ".err" af
      it "shows the same error when compiling this module" $ do
        goldenTextFile (fromAbsFile resultFile) $ do
          (ds, diag) <- runNoLoggingT $ loadModules af
          -- Try to check
          errs <- shouldFailToValidate $ compileDeclarations ds
          pure $ renderValidationErrors diag errs
