{-# LANGUAGE TypeApplications #-}

module Centjes.BlockSpec (spec) where

import Centjes.Block
import Centjes.Block.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = genValidSpec @BlockSize
