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

getCentjesImportRevolutR :: Handler Html
getCentjesImportRevolutR = makeSettingsPage @CLI.Settings "centjes-import-revolut"
