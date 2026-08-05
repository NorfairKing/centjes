{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.ParseSpec (spec) where

import Centjes.Location
import Centjes.Module
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
  parseSpec "transaction" parseTransaction
  parseSpec "declaration" parseDeclaration
  parseSpec "module" parseModule

  -- A run of comment lines formats back out the same way whether it is one
  -- comment or one per line, so only asserting the parsed value can tell them
  -- apart.
  describe "parseModule" $
    it "reads a run of comment lines as one comment declaration" $ do
      here <- getCurrentDir
      m <-
        shouldParse
          parseModule
          here
          [relfile|pure-test.cent|]
          "-- One\n-- Two\ncurrency USD 0.01\n"
      [ locatedValue c
        | Located _ (DeclarationComment c) <- moduleDeclarations m
        ]
        `shouldBe` [Comment "One\nTwo"]

  describe "parseTransaction" $ do
    it "reads a run of indented comment lines as one comment" $ do
      here <- getCurrentDir
      transaction <-
        shouldParse
          parseTransaction
          here
          [relfile|pure-test.cent|]
          "2026-06-05\n  -- One\n  -- Two\n  * assets:cash -5 USD\n"
      map (fmap locatedValue . commentedCommentAbove) (transactionPostings transaction)
        `shouldBe` [Just (Comment "One\nTwo")]

    -- A description is the whole rest of its line, so a double dash in one is
    -- description text rather than a comment.  Formatting cannot show that,
    -- because either way the double dash comes back out in the same place.
    it "reads a double dash on a description line as part of the description" $ do
      here <- getCurrentDir
      transaction <-
        shouldParse
          parseTransaction
          here
          [relfile|pure-test.cent|]
          "2026-06-05\n  | A description -- with a double dash in it"
      (locatedValue . commentedValue <$> transactionDescription transaction)
        `shouldBe` Just (Description "A description -- with a double dash in it")

parseSpec ::
  forall a.
  (HasCallStack) =>
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
        contents <- T.strip <$> T.readFile (fromAbsFile af)
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
            contents <- T.strip <$> T.readFile (fromAbsFile af)
            context (show contents) $
              case parser here rf contents of
                Left err -> pure err
                Right a ->
                  expectationFailure $
                    unlines
                      [ "Should have failed to parse, but got",
                        ppShow a
                      ]
