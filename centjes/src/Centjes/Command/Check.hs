{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Check (runCentjesCheck) where

import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Check
import Centjes.Timing
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Text.Colour
import Text.Colour.Term

runCentjesCheck :: Settings -> CheckSettings -> LoggingT IO ()
runCentjesCheck Settings {..} CheckSettings =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, diagnostic) ->
    withLoggedDuration "Check" $ do
      val <- runValidationT $ doCompleteCheck declarations
      _ <- liftIO $ checkValidation diagnostic val
      liftIO $ putChunksLocale [fore green $ chunk "Valid\n"]
