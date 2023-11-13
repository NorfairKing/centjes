{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.FormatSpec (spec) where

import Centjes.Format
import Centjes.Module.Gen ()
import Centjes.Parse
import Data.Text (Text)
import GHC.Stack
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "formatModule" $ do
    it "can format any module" $
      producesValid formatModule

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
  describe name $
    it "roundtrips" $
      forAllValid $ \expected -> do
        let rendered = formatter (expected :: a)
        context (show rendered) $
          case parser "test-input" rendered of
            Left err -> expectationFailure $ unlines ["Failed to parse:", err]
            Right actual -> (actual :: a) `shouldBe` expected
