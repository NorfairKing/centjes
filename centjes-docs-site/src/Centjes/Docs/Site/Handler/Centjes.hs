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

getCentjesR :: Handler Html
getCentjesR = makeSettingsPage @CLI.Instructions "centjes"

getCentjesCommandR :: Text -> Handler Html
getCentjesCommandR = makeCommandSettingsPage @CLI.Instructions "centjes"
