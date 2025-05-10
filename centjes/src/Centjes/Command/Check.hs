{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Check (runCentjesCheck) where

import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Check
import Centjes.Timing
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

runCentjesCheck :: Settings -> CheckSettings -> LoggingT IO ()
runCentjesCheck Settings {..} CheckSettings =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, diagnostic) ->
    withLoggedDuration "Check" $ do
      val <- runValidationT $ doCompleteCheck declarations
      void $ liftIO $ checkValidation diagnostic val
