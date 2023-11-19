{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.CheckSpec (spec) where

import Centjes.Command.Check
import Centjes.Format
import Centjes.Module
import Centjes.Module.Gen ()
import Centjes.OptParse
import Centjes.Parse
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Path
import Path.IO
import System.Exit
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = modifyMaxSuccess (`div` 10) . tempDirSpec "centjes-check" $ do
  it "can check a given single file without imports" $ \tdir ->
    forAllValid $ \m -> do
      testFile <- resolveFile tdir "test.cent"
      T.writeFile (fromAbsFile testFile) (formatModule (m {moduleImports = []}))
      let settings = Settings {settingLedgerFile = testFile}
      let formatSettings = CheckSettings
      runCentjesCheck settings formatSettings
