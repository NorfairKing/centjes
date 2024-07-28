{-# LANGUAGE TypeApplications #-}

module Centjes.OptParseSpec (spec) where

import Centjes.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Instructions
  goldenSettingsReferenceDocumentationSpec @Instructions "test_resources/documentation.txt" "centjes"
  goldenSettingsNixOptionsSpec @Instructions "options.nix"
