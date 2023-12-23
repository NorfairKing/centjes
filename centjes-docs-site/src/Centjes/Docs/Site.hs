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

centjesDocsSite :: IO ()
centjesDocsSite = do
  Settings {..} <- getSettings
  let app =
        App
          { appAssets = assets,
            appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
          }
  let defMiddles = defaultMiddlewaresNoLogging
  let extraMiddles =
        if development
          then Wai.logStdoutDev
          else Wai.logStdout
  let middle = extraMiddles . defMiddles
  plainApp <- liftIO $ toWaiAppPlain app
  let application = middle plainApp
  Warp.run settingPort application
