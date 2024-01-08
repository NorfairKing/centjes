{-# LANGUAGE TypeApplications #-}

module Centjes.FilterSpec (spec) where

import Centjes.Filter
import Centjes.Filter.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Filter

  describe "filterPredicate" $
    it "does not crash" $
      forAllValid $ \f ->
        forAllValid $ \an ->
          shouldBeValid $ filterPredicate f an
