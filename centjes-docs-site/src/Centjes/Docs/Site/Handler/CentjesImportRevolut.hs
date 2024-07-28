{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportRevolut
  ( getCentjesImportRevolutR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Revolut.OptParse as CLI
import OptEnvConf
import Text.Colour

getCentjesImportRevolutR :: Handler Html
getCentjesImportRevolutR = do
  DocPage {..} <- lookupPage "centjes-import-revolut"
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes-import-revolut" $
            parserDocs $
              settingsParser @CLI.Settings
  defaultLayout $ do
    setCentjesTitle "centjes-import-revolut"
    setDescriptionIdemp "Documentation for the Centjes Importer for Revolut"
    $(widgetFile "args")
