{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VATRate.Gen where

import Centjes.Switzerland.Report.VATRate
import Data.GenValidity

instance GenValid VATRate
