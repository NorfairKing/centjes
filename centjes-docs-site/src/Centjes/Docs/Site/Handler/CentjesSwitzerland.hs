{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesSwitzerland
  ( getCentjesSwitzerlandR,
    getCentjesSwitzerlandCommandR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Switzerland.OptParse as CLI
import Data.Text (Text)
import OptEnvConf
import Text.Colour

getCentjesSwitzerlandR :: Handler Html
getCentjesSwitzerlandR = do
  DocPage {..} <- lookupPage "centjes-switzerland"
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes-switzerland" $
            parserDocs $
              settingsParser @CLI.Settings
  defaultLayout $ do
    setCentjesTitle "centjes-switzerland"
    setDescriptionIdemp "Documentation for the Centjes Reporter for Revolut"
    $(widgetFile "args")

getCentjesSwitzerlandCommandR :: Text -> Handler Html
getCentjesSwitzerlandCommandR cmd = do
  DocPage {..} <- lookupPage' ["centjes-switzerland", cmd]
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes-switzerland" $
            case cmd of
              "taxes" -> parserDocs $ settingsParser @CLI.TaxesSettings
              "vat" -> parserDocs $ settingsParser @CLI.VATSettings
              "download-rates" -> parserDocs $ settingsParser @CLI.VATSettings
              _ -> undefined
  defaultLayout $ do
    setCentjesTitle $ toHtml docPageTitle
    setDescriptionIdemp $ "Documentation for the " <> cmd <> " subcommand of the centjes tool"
    $(widgetFile "args")
