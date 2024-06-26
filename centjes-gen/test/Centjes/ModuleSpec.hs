{-# LANGUAGE TypeApplications #-}

module Centjes.ModuleSpec (spec) where

import Centjes.Module
import Centjes.Module.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(Posting ())
  genValidSpec @(Transaction ())
  genValidSpec @(CurrencyDeclaration ())
  genValidSpec @(AccountDeclaration ())
  genValidSpec @(TagDeclaration ())
  genValidSpec @(PriceDeclaration ())
  genValidSpec @(Declaration ())
  genValidSpec @(Module ())
