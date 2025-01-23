{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportNeon (getCentjesImportNeonR) where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Neon.OptParse as CLI

getCentjesImportNeonR :: Handler Html
getCentjesImportNeonR = makeSettingsPage @CLI.Settings "centjes-import-neon"
