{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportCornercard
  ( getCentjesImportCornercardR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Cornercard.OptParse as CLI
import OptEnvConf
import Text.Colour

getCentjesImportCornercardR :: Handler Html
getCentjesImportCornercardR = do
  DocPage {..} <- lookupPage "centjes-import-cornercard"
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes-import-cornercard" $
            parserDocs $
              settingsParser @CLI.Settings
  defaultLayout $ do
    setCentjesTitle "centjes-import-cornercard"
    setDescriptionIdemp "Documentation for the Centjes Importer for Cornercard"
    $(widgetFile "args")
