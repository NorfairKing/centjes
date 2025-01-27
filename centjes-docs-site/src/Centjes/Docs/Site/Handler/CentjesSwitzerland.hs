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
getCentjesSwitzerlandR = makeSettingsPage @CLI.Instructions "centjes-switzerland"

getCentjesSwitzerlandCommandR :: Text -> Handler Html
getCentjesSwitzerlandCommandR = makeCommandSettingsPage @CLI.Instructions "centjes-switzerland"
