{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.ParseSpec (spec) where

import Centjes.Module.Gen ()
import Centjes.Parse
import Centjes.Parse.TestUtils
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Stack
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  parseSpec "transaction" parseTransaction
  parseSpec "declaration" parseDeclaration
  parseSpec "module" parseModule

parseSpec ::
  forall a.
  HasCallStack =>
  (Show a, GenValid a) =>
  String ->
  (Path Abs Dir -> Path Rel File -> Text -> Either String a) ->
  Spec
parseSpec name parser = withFrozenCallStack $ do
  describe name $ do
    scenarioDir ("test_resources/syntax/" <> name <> "/valid") $ \fp ->
      it (unwords ["can parse", fp]) $ do
        af <- resolveFile' fp
        here <- getCurrentDir
        rf <- makeRelative here af
        contents <- T.readFile (fromAbsFile af)
        context (show contents) $ do
          expected <- shouldParse parser here rf contents
          shouldBeValid expected

    scenarioDir ("test_resources/syntax/" <> name <> "/invalid") $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        errFile <- liftIO $ replaceExtension ".error" af
        it (unwords ["fails to parse", fp, "with the right error"]) $
          goldenStringFile (fromAbsFile errFile) $ do
            here <- getCurrentDir
            rf <- makeRelative here af
            contents <- T.readFile (fromAbsFile af)
            context (show contents) $
              case parser here rf contents of
                Left err -> pure err
                Right a ->
                  expectationFailure $
                    unlines
                      [ "Should have failed to parse, but got",
                        ppShow a
                      ]
