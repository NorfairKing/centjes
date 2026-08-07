{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.CentjesStripe
  ( getCentjesStripeR,
    getCentjesStripeCommandR,
  )
where

import Centjes.Docs.Site.Handler.Import
import Centjes.Stripe.OptParse as CLI
import Data.Text (Text)

getCentjesStripeR :: Handler Html
getCentjesStripeR = makeSettingsPage @CLI.Settings "centjes-stripe"

getCentjesStripeCommandR :: Text -> Handler Html
getCentjesStripeCommandR = makeCommandSettingsPage @CLI.Settings "centjes-stripe"
