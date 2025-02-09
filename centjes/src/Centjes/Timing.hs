{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Timing (withLoggedDuration) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)

withLoggedDuration :: (MonadIO m, MonadLogger m) => String -> m a -> m a
withLoggedDuration name func = do
  begin <- liftIO getMonotonicTimeNSec
  result <- func
  end <- liftIO getMonotonicTimeNSec
  logDebugN $ T.pack $ concat [name, ": ", show @Word64 ((end - begin) `div` 1_000_000), "ms"]
  pure result
