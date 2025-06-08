{-# LANGUAGE TypeApplications #-}

module Centjes.LedgerSpec (spec) where

import Centjes.Ledger
import Centjes.Ledger.Gen ()
import Test.Syd hiding (Assertion)
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @(Currency ())
  genValidSpec @(AmountRatio ())
  genValidSpec @(Cost ())
  genValidSpec @(Price ())
  genValidSpec @(Assertion ())
  genValidSpec @(Posting ())
  genValidSpec @(Transaction ())
  genValidSpec @(Ledger ())
