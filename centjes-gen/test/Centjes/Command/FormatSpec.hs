{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Command.FormatSpec (spec) where

import Centjes.Command.Format
import Centjes.Format
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
spec = modifyMaxSuccess (`div` 10) . tempDirSpec "centjes-format" $ do
  it "can format a given single file" $ \tdir ->
    forAllValid $ \m -> do
      let rf = [relfile|test.foobar|]
      let af = tdir </> rf
      T.writeFile (fromAbsFile af) (formatModule @() m)
      let settings = Settings {settingLedgerFile = af}
      let formatSettings = FormatSettings {formatSettingFileOrDir = Just (Left af)}
      runCentjesFormat settings formatSettings
      assertFormatted tdir rf

  it "can format an entire directory" $ \tdir ->
    forAllValid $ \m1 -> do
      forAllValid $ \m2 -> do
        let rf1 = [relfile|foo.cent|]
        let af1 = tdir </> rf1
        let rf2 = [relfile|bar.cent|]
        let af2 = tdir </> rf2
        T.writeFile (fromAbsFile af1) (formatModule @() m1)
        T.writeFile (fromAbsFile af2) (formatModule @() m2)
        let settings = Settings {settingLedgerFile = af1}
        let formatSettings = FormatSettings {formatSettingFileOrDir = Just (Right tdir)}
        runCentjesFormat settings formatSettings
        assertFormatted tdir rf1
        assertFormatted tdir rf2

  it "Does not format anything if any file fails to parse" $ \tdir -> do
    testFile1 <- resolveFile tdir "foo.cent"
    testFile2 <- resolveFile tdir "bar.cent"
    T.writeFile (fromAbsFile testFile1) "#invalid file"
    let unformatted = "import   foo.cent\n" -- Valid but unformatted
    T.writeFile (fromAbsFile testFile2) unformatted
    let settings = Settings {settingLedgerFile = testFile1}
    let formatSettings = FormatSettings {formatSettingFileOrDir = Just (Right tdir)}
    runCentjesFormat settings formatSettings `shouldThrow` (\(_ :: ExitCode) -> True)
    T.readFile (fromAbsFile testFile2) `shouldReturn` unformatted

assertFormatted :: Path Abs Dir -> Path Rel File -> IO ()
assertFormatted here rf = do
  let af = here </> rf
  contents <- SB.readFile (fromAbsFile af)
  case TE.decodeUtf8' contents of
    Left err -> expectationFailure $ show err
    Right textContents ->
      case parseModule here rf textContents of
        Left err -> expectationFailure err
        Right m -> formatModule m `shouldBe` textContents
