{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Report.TaxesSpec (spec) where

import Centjes.Switzerland.Report.Taxes
import Centjes.Switzerland.Report.Taxes.Gen ()
import Centjes.Switzerland.Reporter
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @TaxesInput
  genValidSpec @(TaxesReport ())

  describe "produceTaxesReport" $
    it "produces valid reports" $
      forAllValid $ \taxesInput -> do
        forAllValid $ \ledger -> do
          let errOrReport = runReporter $ produceTaxesReport @() taxesInput ledger
          shouldBeValid errOrReport
