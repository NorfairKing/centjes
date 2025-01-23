{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Docs.Site.Handler.Page
  ( getPageR,
  )
where

import Centjes.Docs.Site.Foundation
import Data.Text (Text)

getPageR :: [Text] -> Handler Html
getPageR ts = do
  DocPage {..} <- lookupPage' ts
  defaultLayout $ do
    addStylesheet SkylightingCssR
    setCentjesTitle $ toHtml docPageTitle
    setDescriptionIdemp docPageDescription
    $(widgetFile "page")
