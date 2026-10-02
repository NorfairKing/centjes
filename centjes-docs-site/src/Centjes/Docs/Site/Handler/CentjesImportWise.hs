{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportWise (getCentjesImportWiseR) where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Wise.OptParse as CLI

getCentjesImportWiseR :: Handler Html
getCentjesImportWiseR = makeSettingsPage @CLI.Settings "centjes-import-wise"
