{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
      testFile <- resolveFile tdir "test.foobar"
      T.writeFile (fromAbsFile testFile) (formatModule m)
      let settings = Settings {settingLedgerFile = testFile}
      let formatSettings = FormatSettings {formatSettingFileOrDir = Just (Left testFile)}
      runCentjesFormat settings formatSettings
      assertFormatted testFile

  it "can format an entire directory" $ \tdir ->
    forAllValid $ \m1 -> do
      forAllValid $ \m2 -> do
        testFile1 <- resolveFile tdir "foo.cent"
        testFile2 <- resolveFile tdir "bar.cent"
        T.writeFile (fromAbsFile testFile1) (formatModule m1)
        T.writeFile (fromAbsFile testFile2) (formatModule m2)
        let settings = Settings {settingLedgerFile = testFile1}
        let formatSettings = FormatSettings {formatSettingFileOrDir = Just (Right tdir)}
        runCentjesFormat settings formatSettings
        assertFormatted testFile1
        assertFormatted testFile2

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

assertFormatted :: Path Abs File -> IO ()
assertFormatted fp = do
  contents <- SB.readFile (fromAbsFile fp)
  case TE.decodeUtf8' contents of
    Left err -> expectationFailure $ show err
    Right textContents ->
      case parseModule (fromAbsFile fp) textContents of
        Left err -> expectationFailure err
        Right m -> formatModule m `shouldBe` textContents
