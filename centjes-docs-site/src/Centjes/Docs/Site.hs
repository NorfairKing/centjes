{-# LANGUAGE RecordWildCards #-}

module Centjes.Docs.Site
  ( centjesDocsSite,
  )
where

import Centjes.Docs.Site.Application ()
import Centjes.Docs.Site.Constants
import Centjes.Docs.Site.Foundation
import Centjes.Docs.Site.OptParse
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import System.Metrics.Prometheus.GHC.Stats as Prometheus (sampleGhcStats)
import System.Metrics.Prometheus.Wai.Middleware as Prometheus

centjesDocsSite :: IO ()
centjesDocsSite = do
  Settings {..} <- getSettings
  let app =
        App
          { appAssets = assets,
            appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
          }
  registry <- liftIO Registry.new
  metricsEndpoint <-
    metricsEndpointMiddleware $
      Prometheus.withLastSecondSamples (sampleGhcStats mempty) $
        defaultMetricsEndpoint registry
  let logMiddleware =
        if development
          then Wai.logStdoutDev
          else Wai.logStdout
  waiMetrics <- liftIO $ registerWaiMetrics mempty registry
  let middlewares =
        metricsEndpoint
          . instrumentWaiMiddleware waiMetrics
          . logMiddleware
          . defaultMiddlewaresNoLogging
  plainApp <- liftIO $ toWaiAppPlain app
  let application = middlewares plainApp
  Warp.run settingPort application
