{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.AccountType.Gen where

import Centjes.AccountType
import Data.GenValidity

instance GenValid AccountType
