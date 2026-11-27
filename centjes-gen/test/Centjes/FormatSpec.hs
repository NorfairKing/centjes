{-# LANGUAGE OverloadedStrings #-}
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
  let centFilesDirSpec :: Path Rel Dir -> Spec
      centFilesDirSpec dir =
        scenarioDirRecur dir $ \fp -> do
          af <- liftIO $ makeAbsolute (dir </> fp)
          when (fileExtension af == Just ".cent") $
            it "can parse and format idempotently" $
              goldenTextFile (fromRelFile (dir </> fp)) $ do
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
                      pure rendered
  let parseFormatRoundtripWith ::
        forall s.
        (HasCallStack) =>
        ( Show (s ()),
          GenValid (s ()),
          Show (s SourceSpan),
          GenValid (s SourceSpan)
        ) =>
        (Text -> Text) ->
        String ->
        -- [allow:HsNoStringError] The parsers that Happy generates fail with a String.
        (Path Abs Dir -> Path Rel File -> Text -> Either String (s SourceSpan)) ->
        (forall ann. s ann -> Text) ->
        Spec
      parseFormatRoundtripWith func name parser formatter = withFrozenCallStack $ do
        describe name $ do
          validDir <- liftIO $ parseRelDir (concat ["test_resources/syntax/", name, "/valid"])
          scenarioDir validDir $ \fp -> do
            af <- liftIO $ makeAbsolute (validDir </> fp)
            when (fileExtension af == Just ".cent") $ do
              let namedDir = parent (parent af)
              formattedDir <- liftIO $ resolveDir namedDir "formatted"
              let resultFile = formattedDir </> filename af

              it (unwords ["can roundtrip", fromRelFile (validDir </> fp), "back to text the same way"]) $
                goldenTextFile (fromAbsFile resultFile) $ do
                  here <- getCurrentDir
                  rf <- makeRelative here af
                  contents <- func <$> T.readFile (fromAbsFile af)
                  context (show contents) $
                    context (T.unpack contents) $ do
                      expected <- shouldParse parser here rf contents
                      shouldBeValid expected
                      context (ppShow expected) $ do
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
  let parseFormatRoundtrip ::
        forall s.
        (HasCallStack) =>
        ( Show (s ()),
          GenValid (s ()),
          Show (s SourceSpan),
          GenValid (s SourceSpan)
        ) =>
        String ->
        -- [allow:HsNoStringError] The parsers that Happy generates fail with a String.
        (Path Abs Dir -> Path Rel File -> Text -> Either String (s SourceSpan)) ->
        (forall ann. s ann -> Text) ->
        Spec
      parseFormatRoundtrip name parser formatter =
        withFrozenCallStack $ parseFormatRoundtripWith id name parser formatter
  parseFormatRoundtrip "currency" parseCurrencyDeclaration formatCurrencyDeclaration
  parseFormatRoundtrip "account" parseAccountDeclaration formatAccountDeclaration
  parseFormatRoundtrip "tag" parseTagDeclaration formatTagDeclaration
  parseFormatRoundtrip "price" parsePriceDeclaration formatPriceDeclaration
  parseFormatRoundtrip "transaction" parseTransaction formatTransaction
  parseFormatRoundtrip "declaration" parseDeclaration formatDeclaration
  parseFormatRoundtrip "module" parseModule formatModule

  describe "formatModule" $ do
    it "can format any module" $
      producesValid (formatModule @())

    -- These have trailing whitespace that an editor would silently strip from
    -- a test resource file, so they are written out here instead.
    it "keeps the declaration below a comment whose text is only whitespace" $ do
      here <- getCurrentDir
      m <- shouldParse parseModule here [relfile|pure-test.cent|] "--  \ncurrency USD 0.01\n"
      formatModule m `shouldBe` "--  \ncurrency USD 0.01\n"

    -- The marker keeps its space, because a bare "--" is not a comment.
    it "reads back a comment with no text" $ do
      here <- getCurrentDir
      m <- shouldParse parseModule here [relfile|pure-test.cent|] "-- \n"
      formatModule m `shouldBe` "-- \n"

    -- Two empty lines are one comment whose text is a single newline, which
    -- 'Data.Text.lines' cannot tell apart from one empty line.
    it "keeps both lines of a comment of two empty lines" $ do
      here <- getCurrentDir
      m <- shouldParse parseModule here [relfile|pure-test.cent|] "-- \n-- \n"
      formatModule m `shouldBe` "-- \n-- \n"

    it "keeps the whitespace within a comment" $ do
      here <- getCurrentDir
      let contents = "--     Indented, to line up with something.\n-- \tAfter a tab.\n-- Trailing.  \n"
      m <- shouldParse parseModule here [relfile|pure-test.cent|] contents
      formatModule m `shouldBe` contents

  centFilesDirSpec [reldir|test_resources/load|]
  centFilesDirSpec [reldir|test_resources/compile|]
  centFilesDirSpec [reldir|test_resources/balance|]
  centFilesDirSpec [reldir|test_resources/register|]
  centFilesDirSpec [reldir|test_resources/check|]
  centFilesDirSpec [reldir|test_resources/check-valid|]
  centFilesDirSpec [reldir|test_resources/lot|]
  centFilesDirSpec [reldir|test_resources/virtual|]
