{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesImportCornercard (getCentjesImportCornercardR) where

import Centjes.Docs.Site.Handler.Import
import Centjes.Import.Cornercard.OptParse as CLI
import OptEnvConf
import Text.Colour

getCentjesImportCornercardR :: Handler Html
getCentjesImportCornercardR = makeSettingsPage @CLI.Settings "centjes-import-cornercard"
