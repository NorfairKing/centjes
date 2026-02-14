{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesCryptocurrencies
  ( getCentjesCryptocurrenciesR,
    getCentjesCryptocurrenciesCommandR,
  )
where

import Centjes.Cryptocurrencies.OptParse as CLI
import Centjes.Docs.Site.Handler.Import
import Data.Text (Text)

getCentjesCryptocurrenciesR :: Handler Html
getCentjesCryptocurrenciesR = makeSettingsPage @CLI.Settings "centjes-cryptocurrencies"

getCentjesCryptocurrenciesCommandR :: Text -> Handler Html
getCentjesCryptocurrenciesCommandR = makeCommandSettingsPage @CLI.Settings "centjes-cryptocurrencies"
