{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportNeon
  ( getCentjesImportNeonR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Neon.OptParse as CLI
import OptEnvConf
import Text.Colour

getCentjesImportNeonR :: Handler Html
getCentjesImportNeonR = do
  DocPage {..} <- lookupPage "centjes-import-neon"
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes-import-neon" $
            parserDocs $
              settingsParser @CLI.Settings
  defaultLayout $ do
    setCentjesTitle "centjes-import-neon"
    setDescriptionIdemp "Documentation for the Centjes Importer for Neon"
    $(widgetFile "args")
