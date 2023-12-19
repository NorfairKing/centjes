{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.FormatSpec (spec) where

import Centjes.Format
import Centjes.Location
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
  parseFormatRoundtrip "transaction" parseTransaction formatTransaction
  parseFormatRoundtrip "declaration" parseDeclaration formatDeclaration
  parseFormatRoundtrip "module" parseModule formatModule

  describe "formatModule" $ do
    it "can format any module" $
      producesValid (formatModule @())

  centFilesDirSpec "test_resources/balance"
  centFilesDirSpec "test_resources/register"

centFilesDirSpec :: FilePath -> Spec
centFilesDirSpec dir =
  scenarioDirRecur dir $ \fp -> do
    af <- liftIO $ resolveFile' fp
    when (fileExtension af == Just ".cent") $ do
      it (unwords ["can parse and format", fp, "idempotently"]) $ do
        here <- getCurrentDir
        rf <- makeRelative here af
        contents <- T.readFile (fromAbsFile af)
        context (show contents) $ do
          expected <- shouldParse parseModule here rf contents
          shouldBeValid expected
          context (show expected) $ do
            let rendered = formatModule expected
            context (unlines ["Rendered:", T.unpack rendered]) $ do
              actual <- shouldParse parseModule here rf rendered
              formatModule actual `shouldBe` formatModule expected

parseFormatRoundtrip ::
  forall s.
  HasCallStack =>
  ( Show (s ()),
    GenValid (s ()),
    Show (s SourceSpan),
    GenValid (s SourceSpan)
  ) =>
  String ->
  (Path Abs Dir -> Path Rel File -> Text -> Either String (s SourceSpan)) ->
  (forall ann. s ann -> Text) ->
  Spec
parseFormatRoundtrip name parser formatter = withFrozenCallStack $ do
  describe name $ do
    scenarioDir ("test_resources/syntax/" <> name <> "/valid") $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        let namedDir = parent (parent af)
        formattedDir <- liftIO $ resolveDir namedDir "formatted"
        let resultFile = formattedDir </> filename af

        it (unwords ["can roundtrip", fp, "back to text the same way"]) $
          goldenTextFile (fromAbsFile resultFile) $ do
            here <- getCurrentDir
            rf <- makeRelative here af
            contents <- T.readFile (fromAbsFile af)
            context (show contents) $ do
              expected <- shouldParse parser here rf contents
              shouldBeValid expected
              context (show expected) $ do
                let rendered = formatter (expected :: (s SourceSpan))
                context (unlines ["Rendered:", T.unpack rendered]) $ do
                  actual <- shouldParse parser here rf rendered
                  formatter (actual :: (s SourceSpan)) `shouldBe` formatter expected
                  pure (formatter actual)

    it "roundtrips valid values back to text the same way" $
      forAllValid $ \expected -> do
        let rendered = formatter (expected :: (s ()))
        here <- getCurrentDir
        context (unlines ["Rendered:", T.unpack rendered, show rendered]) $ do
          actual <- shouldParse parser here [relfile|pure-test.cent|] rendered
          context (ppShow actual) $ do
            formatter (actual :: (s SourceSpan)) `shouldBe` formatter expected
