{-# LANGUAGE TypeApplications #-}

module Centjes.LocationSpec (spec) where

import Centjes.Location
import Centjes.Location.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @SourcePosition
  genValidSpec @SourceSpan
  genValidSpec @(GenLocated SourceSpan Int)
