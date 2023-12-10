{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Description.Gen where

import Centjes.Description as Description
import Data.GenValidity
import Data.GenValidity.Text ()

instance GenValid Description
