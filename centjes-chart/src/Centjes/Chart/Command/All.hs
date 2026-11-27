{-# LANGUAGE RecordWildCards #-}

module Centjes.Chart.Command.All (runCentjesChartAll) where

import Centjes.AccountType (AccountType (..))
import Centjes.Chart.OptParse
import Centjes.Chart.Render
import Centjes.Chart.Report
import Centjes.Compile
import Centjes.Load
import Centjes.Report.EvaluatedLedger
import Centjes.Validation
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger

runCentjesChartAll :: Settings -> AllSettings -> LoggingT IO ()
runCentjesChartAll Settings {..} AllSettings {..} =
  loadMWatchedModules settingsWatch settingsLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    evaluatedLedger <- liftIO $ checkValidation diagnostic $ produceEvaluatedLedger ledger
    report <-
      liftIO $
        checkValidation diagnostic $
          produceChartReport [AccountTypeAssets] allSettingsFilter allSettingsCurrency evaluatedLedger
    let renderSettings =
          RenderSettings
            { renderSettingsTitle = allSettingsTitle,
              renderSettingsWidth = 1920,
              renderSettingsHeight = 1080
            }
    liftIO $ renderChartReportSvg renderSettings report allSettingsOutputFile
