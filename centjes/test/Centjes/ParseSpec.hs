{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.ParseSpec (spec) where

import Centjes.Module.Gen ()
import Centjes.Parse
import Centjes.Parse.TestUtils
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let parseSpec' n p = parseSpec n (\fp t -> p fp (T.strip t))
  parseSpec' "account" parseAccount
  parseSpec' "account-name" parseAccountName
  parseSpec "posting" parsePosting
  parseSpec "transaction" parseTransaction
  parseSpec "import" parseImport
  parseSpec "declaration" parseDeclaration
  parseSpec "module" parseModule

parseSpec ::
  forall a.
  HasCallStack =>
  (Show a, GenValid a) =>
  String ->
  (FilePath -> Text -> Either String a) ->
  Spec
parseSpec name parser = withFrozenCallStack $ do
  describe name $ do
    scenarioDir ("test_resources/" <> name <> "/valid") $ \fp ->
      it (unwords ["can parse", fp]) $ do
        contents <- T.readFile fp
        expected <- shouldParse parser fp contents
        shouldBeValid expected

    scenarioDir ("test_resources/" <> name <> "/invalid") $ \fp -> do
      af <- liftIO $ resolveFile' fp
      errFile <- liftIO $ replaceExtension ".error" af
      when (fileExtension af == Just ".cent") $
        it (unwords ["fails to parse", fp, "with the right error"]) $
          goldenStringFile (fromAbsFile errFile) $ do
            contents <- T.readFile fp
            case parser fp contents of
              Left err -> pure err
              Right a ->
                expectationFailure $
                  unlines
                    [ "Should have failed to parse, but got",
                      ppShow a
                    ]
