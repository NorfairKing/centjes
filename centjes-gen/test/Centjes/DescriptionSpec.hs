{-# LANGUAGE TypeApplications #-}

module Centjes.DescriptionSpec (spec) where

import Centjes.Description
import Centjes.Description.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Description
