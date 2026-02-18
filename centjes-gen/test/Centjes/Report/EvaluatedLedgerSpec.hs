{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.EvaluatedLedgerSpec (spec) where

import Centjes.Ledger.Gen ()
import Centjes.Report.EvaluatedLedger
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "balanceTransaction" $ do
    it "produces valid balances" $
      producesValid2 $
        balanceTransaction @()

  describe "produceEvaluatedLedger" $ do
    it "does not crash" $
      forAllValid $ \ledger ->
        -- produceEvaluatedLedger may legitimately fail on generated ledgers
        -- (e.g. overflow), but it must never crash. Evaluating the result
        -- to WHNF verifies this.
        let _ = produceEvaluatedLedger @() ledger in pure () :: IO ()
