{-# LANGUAGE RecordWildCards #-}

module Centjes.Stripe (runCentjesStripe) where

import Centjes.Stripe.Command.Import
import Centjes.Stripe.OptParse

runCentjesStripe :: IO ()
runCentjesStripe = do
  settings@Settings {..} <- getSettings
  case settingCommand of
    CommandImport importSettings ->
      runCentjesStripeImport settings importSettings
