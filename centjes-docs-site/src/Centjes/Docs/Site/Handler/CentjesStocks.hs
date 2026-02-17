{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesStocks
  ( getCentjesStocksR,
    getCentjesStocksCommandR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Stocks.OptParse as CLI
import Data.Text (Text)

getCentjesStocksR :: Handler Html
getCentjesStocksR = makeSettingsPage @CLI.Settings "centjes-stocks"

getCentjesStocksCommandR :: Text -> Handler Html
getCentjesStocksCommandR = makeCommandSettingsPage @CLI.Settings "centjes-stocks"
