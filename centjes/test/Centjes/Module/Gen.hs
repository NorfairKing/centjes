{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Module.Gen where

import Centjes.Module
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Money.Account.Gen ()

instance GenValid Module

instance GenValid Declaration

instance GenValid Transaction

instance GenValid Posting
