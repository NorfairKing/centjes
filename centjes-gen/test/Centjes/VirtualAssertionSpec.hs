{-# LANGUAGE OverloadedStrings #-}

module Centjes.VirtualAssertionSpec (spec) where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Location
import Centjes.Report.EvaluatedLedger
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import qualified Data.Vector as V
import Path.IO
import Test.Syd

spec :: Spec
spec =
  describe "checkEvaluatedLedgerAssertions" $
    -- On a virtual-allowed account the two scopes give different answers, which
    -- is the only case where the choice carries information.
    it "checks each assertion against the balance its scope names" $ do
      af <- liftIO $ resolveFile' "test_resources/virtual/both-scopes.cent"
      (ds, diag) <- runNoLoggingT $ loadModules af
      ledger <- shouldValidate diag $ compileDeclarations ds
      evaluatedLedger <- shouldValidate diag $ produceEvaluatedLedger ledger

      [ (scope, currencySymbolText (currencySymbol (commodityCurrency commodity)))
        | Located _ transaction <- V.toList (ledgerTransactions ledger),
          Located _ (AssertionEquals scope _ _ (Located _ commodity)) <-
            V.toList (transactionAssertions transaction)
        ]
        `shouldBe` [ (AssertionScopeReal, "CHF"),
                     (AssertionScopeVirtual, "CHF")
                   ]

      shouldValidate diag $ checkEvaluatedLedgerAssertions evaluatedLedger
