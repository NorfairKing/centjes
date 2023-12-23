{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Docs.Site.Handler.Nix where

import Centjes.Docs.Site.Handler.Import
import Centjes.Docs.Site.ModuleDocs
import Language.Haskell.TH.Load

getNixosModuleR :: Handler Html
getNixosModuleR = do
  options <- loadIO nixosModuleDocs
  let title = "Nixos Module Reference"
  let description = "Generated reference documentation about the nixos module for centjes server deployments."
  defaultLayout $ do
    setTitle title
    setDescriptionIdemp description
    let optionDocs = $(widgetFile "option-docs")
    $(widgetFile "nixos-module")
