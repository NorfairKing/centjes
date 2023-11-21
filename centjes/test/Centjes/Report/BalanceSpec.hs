{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Ledger.Gen ()
import Centjes.Report.Balance
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "produceBalanceReport" $
    it "produces valid reports" $
      producesValid produceBalanceReport
