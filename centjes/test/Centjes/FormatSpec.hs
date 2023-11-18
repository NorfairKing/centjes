{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.FormatSpec (spec) where

import Centjes.Format
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
    scenarioDir ("test_resources/" <> name <> "/valid") $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        let namedDir = parent (parent af)
        formattedDir <- liftIO $ resolveDir namedDir "formatted"
        let resultFile = formattedDir </> filename af

        it (unwords ["can roundtrip", fp, "back to text the same way"]) $
          goldenTextFile (fromAbsFile resultFile) $ do
            contents <- T.readFile fp
            expected <- shouldParse parser fp contents
            shouldBeValid expected
            context (show expected) $ do
              let rendered = formatter (expected :: a)
              context (unlines ["Rendered:", T.unpack rendered]) $ do
                actual <- shouldParse parser "test-input" rendered
                formatter (actual :: a) `shouldBe` formatter expected
                pure (formatter actual)

    it "roundtrips valid values back to text the same way" $
      forAllValid $ \expected -> do
        let rendered = formatter (expected :: a)
        context (unlines ["Rendered:", T.unpack rendered]) $ do
          actual <- shouldParse parser "test-input" rendered
          context (ppShow actual) $ do
            formatter (actual :: a) `shouldBe` formatter expected
