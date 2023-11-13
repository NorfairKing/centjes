{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.FormatSpec (spec) where

import Centjes.Format
import Centjes.Module.Gen ()
import Centjes.Parse
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "formatModule" $ do
    it "can format any module" $
      producesValid formatModule

    parseFormatRoundtrip "amount" parseAmount formatAmount
    parseFormatRoundtrip "account" parseAccount formatAccount
    parseFormatRoundtrip "account-name" parseAccountName formatAccountName
    parseFormatRoundtrip "posting" parsePosting formatPosting
    parseFormatRoundtrip "transaction" parseTransaction formatTransaction
    parseFormatRoundtrip "declaration" parseDeclaration formatDeclaration
    parseFormatRoundtrip "module" parseModule formatModule

parseFormatRoundtrip ::
  forall a.
  HasCallStack =>
  (Show a, Eq a, GenValid a) =>
  String ->
  (FilePath -> Text -> Either String a) ->
  (a -> Text) ->
  Spec
parseFormatRoundtrip name parser formatter = withFrozenCallStack $ do
  describe name $ do
    describe "can parse the examples" $
      scenarioDir ("test_resources/" <> name) $ \fp ->
        it ("can parse " <> fp <> "and roundtrip it") $ do
          contents <- T.strip <$> T.readFile fp
          case parser fp contents of
            Left err -> expectationFailure $ unlines ["Failed to parse:", err]
            Right expected -> do
              shouldBeValid expected
              context (show expected) $ do
                let rendered = formatter (expected :: a)
                context (unlines ["Rendered:", show rendered]) $
                  case parser "test-input" rendered of
                    Left err -> expectationFailure $ unlines ["Failed to parse:", err]
                    Right actual -> (actual :: a) `shouldBe` expected

    it "roundtrips" $
      forAllValid $ \expected -> do
        let rendered = formatter (expected :: a)
        context (unlines ["Rendered:", show rendered]) $
          case parser "test-input" rendered of
            Left err -> expectationFailure $ unlines ["Failed to parse:", err]
            Right actual -> (actual :: a) `shouldBe` expected
