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
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    evaluatedLedger <- liftIO $ checkValidation diagnostic $ produceEvaluatedLedger ledger
    report <-
      liftIO $
        checkValidation diagnostic $
          produceChartReport [AccountTypeAssets] allSettingFilter allSettingCurrency evaluatedLedger
    let renderSettings =
          RenderSettings
            { renderSettingTitle = allSettingTitle,
              renderSettingWidth = 1920,
              renderSettingHeight = 1080
            }
    liftIO $ renderChartReportSvg renderSettings report allSettingOutputFile
