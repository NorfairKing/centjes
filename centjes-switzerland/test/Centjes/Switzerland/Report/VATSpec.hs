{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Report.VATSpec (spec) where

import Centjes.Switzerland.Report.VAT
import Centjes.Switzerland.Report.VAT.Gen ()
import Centjes.Switzerland.Reporter
import Centjes.Validation
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(VATReport ())
  describe "produceVATReport" $
    it "produces valid reports" $
      forAllValid $ \vatInput -> do
        forAllValid $ \ledger -> do
          errOrReport <- runValidationT $ runReporter $ produceVATReport @() vatInput ledger
          shouldBeValid errOrReport
