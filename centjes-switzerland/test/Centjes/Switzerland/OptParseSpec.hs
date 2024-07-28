{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.OptParseSpec (spec) where

import Centjes.Switzerland.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "test_resources/documentation.txt" "centjes-switzerland"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
