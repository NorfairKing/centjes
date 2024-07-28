{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.Centjes
  ( getCentjesR,
    getCentjesCommandR,
  )
where

import Centjes.Docs.Site.Handler.Import hiding (Header)
import Centjes.OptParse as CLI
import Data.Text (Text)
import OptEnvConf
import Text.Colour

getCentjesR :: Handler Html
getCentjesR = do
  DocPage {..} <- lookupPage "centjes"
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes" $
            parserDocs $
              settingsParser @CLI.Instructions
  defaultLayout $ do
    setCentjesTitle "centjes"
    setDescriptionIdemp "Documentation for the Centjes tool"
    $(widgetFile "args")

getCentjesCommandR :: Text -> Handler Html
getCentjesCommandR cmd = do
  DocPage {..} <- lookupPage' ["centjes", cmd]
  let optionsReferenceDocs =
        renderChunksText WithoutColours $
          renderReferenceDocumentation "centjes" $
            case cmd of
              "check" -> parserDocs $ settingsParser @CLI.CheckSettings
              "register" -> parserDocs $ settingsParser @CLI.RegisterSettings
              "balance" -> parserDocs $ settingsParser @CLI.BalanceSettings
              "format" -> parserDocs $ settingsParser @CLI.FormatSettings
              _ -> undefined
  defaultLayout $ do
    setCentjesTitle $ toHtml docPageTitle
    setDescriptionIdemp $ "Documentation for the " <> cmd <> " subcommand of the centjes tool"
    $(widgetFile "args")
