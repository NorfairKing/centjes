{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Centjes.Docs.Site.Foundation
  ( module Centjes.Docs.Site.Foundation,
    module Centjes.Docs.Site.Assets,
    module Centjes.Docs.Site.Static,
    module Centjes.Docs.Site.Widget,
    module Yesod,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Centjes.Docs.Site.Assets
import Centjes.Docs.Site.Constants
import Centjes.Docs.Site.Static
import Centjes.Docs.Site.Widget
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Load
import Text.Hamlet
import Yesod
import Yesod.AutoReload
import Yesod.EmbeddedStatic

data App = App
  { appAssets :: !EmbeddedStatic,
    appGoogleAnalyticsTracking :: !(Maybe Text),
    appGoogleSearchConsoleVerification :: !(Maybe Text)
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  defaultLayout widget = do
    app <- getYesod
    let addReloadWidget = if development then (<> autoReloadWidgetFor ReloadR) else id
    pageContent <-
      widgetToPageContent $ do
        addScript $ AssetsStaticR asciinema_player_js
        addStylesheet $ AssetsStaticR asciinema_player_css
        let menu = $(widgetFile "menu")
        addReloadWidget $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")
  errorHandler NotFound = fmap toTypedContent $
    defaultLayout $
      do
        setTitle "Page not found"
        [whamlet|
      <h1>
        Page not found
      |]
  errorHandler other = defaultErrorHandler other

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Centjes Documentation"
  setDescriptionIdemp "Documentation for Centjes"
  $(widgetFile "home")

getAssetsR :: [Text] -> Handler Html
getAssetsR t = do
  neverExpires
  redirect $ T.intercalate "/" $ "/assets-static/res" : t

lookupPage :: Text -> Handler DocPage
lookupPage url = do
  dps <- loadDocPages
  case M.lookup [url] dps of
    Nothing -> notFound
    Just dp -> pure dp

lookupPage' :: [Text] -> Handler DocPage
lookupPage' url = do
  dps <- loadDocPages
  case M.lookup url dps of
    Nothing -> notFound
    Just dp -> pure dp

setCentjesTitle :: MonadWidget m => Html -> m ()
setCentjesTitle t = setTitle $ "Centjes Documentation - " <> t

loadDocPages :: MonadHandler m => m (Map [Text] DocPage)
loadDocPages = loadIO docPages

yamlDesc :: forall a. HasCodec a => Text
yamlDesc = yamlDescVia (codec @a)

yamlDescVia :: forall a. JSONCodec a -> Text
yamlDescVia = renderPlainSchemaVia
