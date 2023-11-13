module Centjes.FormatSpec (spec) where

import Centjes.Format
-- import Centjes.Module
import Centjes.Module.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "formatSpec" $
    it "can format any module" $
      producesValid formatModule
