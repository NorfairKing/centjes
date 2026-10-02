{-# LANGUAGE TypeApplications #-}

module Centjes.Import.Wise.OptParseSpec (spec) where

import Centjes.Import.Wise.OptParse
import OptEnvConf.Test
import Test.Syd

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "centjes-import-wise"
