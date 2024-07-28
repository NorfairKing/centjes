{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.OptParseSpec (spec) where

import Centjes.Docs.Site.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  describe "Settings" $ do
    settingsLintSpec @Settings
    goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "centjes-docs-site"
    goldenSettingsNixOptionsSpec @Settings "options.nix"
