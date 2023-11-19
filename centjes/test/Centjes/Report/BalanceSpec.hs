{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Compile
import Centjes.Ledger.Gen ()
import qualified Centjes.Module as Module
import Centjes.Report.Balance
import Centjes.Validation
import qualified Data.Map.Strict as M
import Test.QuickCheck (forAll)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "produceBalanceReport" $
    it "produces valid reports" $
      producesValid produceBalanceReport
