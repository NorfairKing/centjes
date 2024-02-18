{-# LANGUAGE TypeApplications #-}

module Centjes.TagSpec (spec) where

import Centjes.Tag as Tag
import Centjes.Tag.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Tag

  describe "fromText" $
    it "produces valid currency symbols" $
      producesValid Tag.fromText
