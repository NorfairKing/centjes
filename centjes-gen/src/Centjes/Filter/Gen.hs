{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Filter.Gen where

import Centjes.AccountName.Gen ()
import Centjes.Filter
import Data.GenValidity
import Data.GenValidity.Text ()

instance GenValid Filter
