{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.FormatSpec (spec) where

import Centjes.Format
import Centjes.Module.Gen ()
import Centjes.Parse
import Centjes.Parse.Alex
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let parseFormatRoundtrip' n p = parseFormatRoundtrip n (\fp t -> p fp (T.strip t))
  parseFormatRoundtrip' "account" parseAccount formatAccount
  parseFormatRoundtrip' "account-name" parseAccountName formatAccountName
  parseFormatRoundtrip "posting" parsePosting formatPosting
  parseFormatRoundtrip "transaction" parseTransaction formatTransaction
  parseFormatRoundtrip "import" parseImport formatImport
  parseFormatRoundtrip "declaration" parseDeclaration formatDeclaration
  parseFormatRoundtrip "module" parseModule formatModule

  describe "formatModule" $ do
    it "can format any module" $
      producesValid formatModule

parseFormatRoundtrip ::
  forall a.
  HasCallStack =>
  (Show a, GenValid a) =>
  String ->
  (FilePath -> Text -> Either String a) ->
  (a -> Text) ->
  Spec
parseFormatRoundtrip name parser formatter = withFrozenCallStack $ do
  describe name $ do
    scenarioDir ("test_resources/" <> name <> "/invalid") $ \fp ->
      it (unwords ["fails to parse", fp]) $ do
        contents <- T.readFile fp
        case parser fp contents of
          Left _ -> pure () -- TODO make this a golden test so we can get good errors
          Right a ->
            expectationFailure $
              unlines
                [ "Should have failed to parse, but got",
                  ppShow a
                ]

    scenarioDir ("test_resources/" <> name <> "/valid") $ \fp ->
      it (unwords ["can parse", fp, "and roundtrip it back to text the same way"]) $ do
        contents <- T.readFile fp
        expected <- shouldParse parser fp contents
        shouldBeValid expected
        context (show expected) $ do
          let rendered = formatter (expected :: a)
          context (unlines ["Rendered:", T.unpack rendered]) $ do
            actual <- shouldParse parser "test-input" rendered
            -- TODO make this a golden test
            formatter (actual :: a) `shouldBe` formatter expected

    it "roundtrips valid values back to text the same way" $
      forAllValid $ \expected -> do
        let rendered = formatter (expected :: a)
        context (unlines ["Rendered:", T.unpack rendered]) $ do
          actual <- shouldParse parser "test-input" rendered
          context (ppShow actual) $ do
            formatter (actual :: a) `shouldBe` formatter expected

shouldParse :: (FilePath -> Text -> Either String a) -> FilePath -> Text -> IO a
shouldParse parser fp contents =
  case parser fp contents of
    Left err -> do
      expectationFailure $
        unlines $
          concat
            [ [ "Failed to parse:",
                err
              ],
              [ "",
                "tokens:",
                ppShow (scanMany (T.unpack contents))
              ]
            ]
    Right m -> pure m
