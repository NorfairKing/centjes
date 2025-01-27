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

import Centjes.Docs.Site.Assets
import Centjes.Docs.Site.Constants
import Centjes.Docs.Site.Static
import Centjes.Docs.Site.Widget
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Load
import qualified OptEnvConf
import qualified OptEnvConf.Args as OptEnvConf
import Text.Colour
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
        toWidget [lucius|:root {--bulma-code: #353535 !important}|]
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

setCentjesTitle :: (MonadWidget m) => Html -> m ()
setCentjesTitle t = setTitle $ "Centjes Documentation - " <> t

loadDocPages :: (MonadHandler m) => m (Map [Text] DocPage)
loadDocPages = loadIO docPages

makeSettingsPage :: forall a. (OptEnvConf.HasParser a) => String -> Handler Html
makeSettingsPage progname = do
  DocPage {..} <- lookupPage $ T.pack progname
  defaultLayout $ do
    let p = OptEnvConf.settingsParser :: OptEnvConf.Parser a
    let docs = OptEnvConf.parserDocs p
    let docsChunks = OptEnvConf.renderReferenceDocumentation progname docs
    let render = renderChunksText WithoutColours
    let referenceDocs = render docsChunks
    let renderedOptDocs = render $ OptEnvConf.renderLongOptDocs $ OptEnvConf.docsToOptDocs docs
    let renderedEnvDocs = render $ OptEnvConf.renderEnvDocs $ OptEnvConf.docsToEnvDocs docs
    let renderedConfDocs = render $ OptEnvConf.renderConfDocs $ OptEnvConf.docsToConfDocs docs
    setCentjesTitle $ toHtml docPageTitle
    setDescriptionIdemp docPageDescription
    $(widgetFile "settings")

makeCommandSettingsPage :: forall a. (OptEnvConf.HasParser a) => String -> Text -> Handler Html
makeCommandSettingsPage progname command = do
  DocPage {..} <- lookupPage' [T.pack progname, command]

  errOrHelpDoc <-
    liftIO $
      OptEnvConf.runHelpParser
        Nothing
        (OptEnvConf.parseArgs [T.unpack command])
        (OptEnvConf.settingsParser :: OptEnvConf.Parser a)
  case errOrHelpDoc of
    Left err -> error $ show err -- Will be caught by yesod
    Right Nothing -> error "Command not found"
    Right (Just (path, cDoc)) -> do
      let docsChunks = OptEnvConf.renderCommandHelpPage progname path cDoc
      let docs = OptEnvConf.commandDocs cDoc

      defaultLayout $ do
        let render = renderChunksText WithoutColours
        let referenceDocs = render docsChunks
        let optDocs = OptEnvConf.docsToOptDocs docs
        let renderedOptDocs =
              render $
                concat
                  [ OptEnvConf.renderShortOptDocs (unwords [progname, T.unpack command]) optDocs,
                    ["\n\n"],
                    OptEnvConf.renderLongOptDocs optDocs
                  ]
        let renderedEnvDocs = render $ OptEnvConf.renderEnvDocs $ OptEnvConf.docsToEnvDocs docs
        let renderedConfDocs = render $ OptEnvConf.renderConfDocs $ OptEnvConf.docsToConfDocs docs
        setCentjesTitle $ toHtml docPageTitle
        setDescriptionIdemp docPageDescription
        $(widgetFile "settings")
