{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Docs.Site.Static.TH where

import CMark as MD
import Centjes.Format
import Centjes.Parse
import Data.Data
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax
import Path
import Prettyprinter
import Prettyprinter.Render.Util.SimpleDocTree as Prettyprinter
import Skylighting
import qualified System.FilePath as FP
import Text.Blaze.Html (Html, preEscapedToHtml, (!))
import qualified Text.Blaze.Html as B
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as HtmlA

data DocPage = DocPage
  { docPageTitle :: !Text,
    docPageDescription :: !Text,
    docPageAttributes :: ![(Text, Text)],
    docPageContents :: !Text,
    docPageRendered :: !Text
  }
  deriving (Typeable, Lift)

docPageKeyFunc :: Path Rel File -> [Text]
docPageKeyFunc = map T.pack . FP.splitDirectories . FP.dropExtension . toFilePath

docPageValFunc :: [Text] -> Text -> DocPage
docPageValFunc urlPieces rawContents =
  let (attributes, contents) = splitContents rawContents
      maybeAtt k =
        case lookup k attributes of
          Nothing ->
            error $
              unlines
                [ "The post with url",
                  show urlPieces,
                  "Does not have an attribute with key",
                  T.unpack k
                ]
          Just a -> a
      title = maybeAtt "title"
      description = maybeAtt "description"
      rendered = renderMarkdown contents
   in DocPage
        { docPageTitle = title,
          docPageDescription = description,
          docPageAttributes = attributes,
          docPageContents = contents,
          docPageRendered = rendered
        }

renderMarkdown :: Text -> Text
renderMarkdown contents =
  let opts = [optUnsafe]
      n = commonmarkToNode opts contents :: Node
   in renderNode n

smallestHeadingLevel :: Node -> Int
smallestHeadingLevel = getMin . go
  where
    go :: Node -> Min Int
    go (Node _ nt subs) =
      ( case nt of
          HEADING level -> Min level
          _ -> mempty
      )
        <> foldMap go subs

renderNode :: Node -> Text
renderNode topLevel = LT.toStrict . renderHtml $ go topLevel
  where
    minHeadingLevel = smallestHeadingLevel topLevel
    go :: Node -> Html
    go n@(Node _ nt subs) = case nt of
      DOCUMENT -> foldMap go subs
      PARAGRAPH -> Html.p (foldMap go subs)
      TEXT t -> B.text t <> foldMap go subs
      HEADING level ->
        let wrappingTag =
              -- We normalise the html heading levels here
              -- We need h1 for the heading, so we want to start at h), which
              -- is where the + 2 comes from.
              let pretendLevel = level - minHeadingLevel + 2
               in case pretendLevel of
                    1 -> Html.h1
                    2 -> Html.h2
                    3 -> Html.h3
                    4 -> Html.h4
                    5 -> Html.h5
                    _ -> Html.b
         in wrappingTag $ foldMap go subs
      HTML_INLINE code -> preEscapedToHtml code <> foldMap go subs
      HTML_BLOCK code -> preEscapedToHtml code <> foldMap go subs
      SOFTBREAK -> " "
      LINEBREAK -> Html.br
      CODE code -> Html.code $ B.text code
      BLOCK_QUOTE -> Html.blockquote $ foldMap go subs
      STRONG -> Html.b $ foldMap go subs
      EMPH -> Html.em $ foldMap go subs
      -- Here's the custom image handling
      IMAGE url title ->
        if not (T.null title)
          then error $ show title
          else
            Html.div ! HtmlA.class_ "columns is-centered" $
              Html.div ! HtmlA.class_ "column has-text-centered" $
                Html.img ! HtmlA.src (B.textValue url) ! HtmlA.alt (B.textValue (foldMap scrubNode subs))
      LINK url title ->
        if not (T.null title)
          then error $ show title
          else Html.a ! HtmlA.href (B.textValue url) $ foldMap go subs
      LIST attrs ->
        let node = case listType attrs of
              BULLET_LIST -> Html.ul
              ORDERED_LIST -> Html.ol
         in node $ foldMap go subs
      ITEM -> Html.li $ foldMap go subs
      -- Here's the custom server-side syntax highlighting.
      CODE_BLOCK language code ->
        -- Supported languages here: https://github.com/jgm/skylighting/tree/master/skylighting-core/xml
        case language of
          "" -> error $ unlines ["Code block must be annotated with a programming language:", T.unpack code]
          "plain" -> Html.pre $ B.text code
          "console" -> Html.pre $ B.text code
          "centjes" ->
            let fakeBaseDir = [absdir|/home/user/finances|]
                fakeFile = [relfile|ledger.cent|]
             in case parseModule fakeBaseDir fakeFile code of
                  Left err -> error $ unlines ["Could not parse module:", err, T.unpack code]
                  Right lmodule -> renderHtmlDoc $ moduleDoc lmodule
          _ ->
            let tokenizerConfig = TokenizerConfig {syntaxMap = defaultSyntaxMap, traceOutput = False}
                syntax = case syntaxByName (syntaxMap tokenizerConfig) language of
                  Nothing -> error $ "Unknown programming language for highlighting: " <> show language
                  Just s -> s
                sourceLines = case tokenize tokenizerConfig syntax code of
                  Left err -> error $ "Failed to parse source code: " <> err
                  Right sls -> sls
             in Html.div ! HtmlA.style (B.textValue $ T.pack $ styleToCss espresso) $
                  Skylighting.formatHtmlBlock Skylighting.defaultFormatOpts sourceLines
      _ -> error $ "Unsupported node: " <> show n

renderHtmlDoc :: Doc SyntaxElement -> Html
renderHtmlDoc doc =
  Html.div ! HtmlA.style (B.textValue $ T.pack $ styleToCss espresso) $
    Html.div ! HtmlA.class_ "sourceCode" $
      Html.pre ! HtmlA.class_ "sourceCode" $
        Html.code ! HtmlA.class_ "sourceCode" $
          foldMap go $
            treeUp . Prettyprinter.treeForm $
              layoutPretty layoutOptions doc
  where
    layoutOptions = LayoutOptions {layoutPageWidth = Unbounded}
    go :: DocTree SyntaxElement -> Html
    go = \case
      DTText t -> Html.span $ Html.text t
      DTAnnotated ann ts ->
        Html.span ! HtmlA.class_ (syntaxElementClass ann) $
          foldMap go ts
    syntaxElementClass :: SyntaxElement -> Html.AttributeValue
    syntaxElementClass = \case
      SyntaxImport -> "im"
      SyntaxKeyword -> "kw"
      SyntaxComment -> "co"
      SyntaxDecimalLiteral -> "dv"
      SyntaxTimestamp -> "dt"
      SyntaxDescription -> "vs"
      SyntaxCurrencySymbol -> "st"
      SyntaxAccountName -> "va"

data DocTree ann
  = DTText !Text
  | DTAnnotated !ann ![DocTree ann]

treeUp :: SimpleDocTree ann -> [DocTree ann]
treeUp = go . pure
  where
    go :: [SimpleDocTree ann] -> [DocTree ann]
    go = \case
      [] -> []
      (STEmpty : rest) -> go rest -- Should not happen.
      (STChar c : rest) -> DTText (T.pack [c]) : go rest
      (STText _ t : rest) -> DTText t : go rest
      (STLine i : rest) -> DTText (T.pack $ concat $ "\n" : replicate i "  ") : go rest
      (STAnn ann sdt : rest) -> DTAnnotated ann (go [sdt]) : go rest
      (STConcat sdts : rest) -> go $ sdts ++ rest

scrubNode :: Node -> Text
scrubNode = go
  where
    go :: Node -> Text
    go (Node _ typ nodes) =
      ( case typ of
          TEXT t -> t
          CODE t -> t
          LINK _ t -> t
          IMAGE _ t -> t
          SOFTBREAK -> " "
          _ -> ""
      )
        <> foldMap go nodes

splitContents :: Text -> ([(Text, Text)], Text)
splitContents cs =
  let threeDashes = "---"
      parts = T.splitOn threeDashes cs
   in case parts of
        "" : ts : rest ->
          let attLines = T.lines ts
              tags =
                flip mapMaybe attLines $ \l ->
                  let col = ":"
                   in case T.splitOn col l of
                        [] -> Nothing
                        (key : valParts) ->
                          Just (key, T.intercalate col valParts)
              contents = T.intercalate threeDashes rest
           in (tags, contents)
        [contents] -> ([], contents)
        _ -> error $ "Failed to parse attributes in" <> T.unpack cs
