{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Centjes
  ( runCentjes,
  )
where

import Brick
import Control.Monad.IO.Class
import Control.Monad.ST
import Control.Monad.State
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Data.Word
import GHC.Generics (Generic)
import System.Random
import Text.Printf

runCentjes :: IO ()
runCentjes = pure ()
