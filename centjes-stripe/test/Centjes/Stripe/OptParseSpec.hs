{-# LANGUAGE TypeApplications #-}

module Centjes.Stripe.OptParseSpec (spec) where

import Centjes.Stripe.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "centjes-stripe"
  goldenSettingsNixOptionsSpec @Settings "options.nix"
