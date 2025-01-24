{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Docs.Site.Handler.Syntax
  ( getSyntaxR,
  )
where

import qualified Centjes.AccountName as AccountName
import qualified Centjes.AccountType as AccountType
import Centjes.Docs.Site.Handler.Import hiding (Header)
import Centjes.Format
import Centjes.Location
import Centjes.Module
import Data.Text (Text)
import Path

getSyntaxR :: Handler Html
getSyntaxR = do
  defaultLayout $ do
    setCentjesTitle "Centjes syntax"
    setDescriptionIdemp "syntax of the centjes dsl"
    $(widgetFile "syntax")
  where
    importExample = Import {importFile = noLoc [relfile|banks/ubs|]}
    spliceImportExample :: Import () -> Widget
    spliceImportExample = spliceSyntaxExample formatImport
    spliceDeclarationExample :: Declaration () -> Widget
    spliceDeclarationExample = spliceSyntaxExample formatDeclaration
    spliceSyntaxExample :: (a -> Text) -> a -> Widget
    spliceSyntaxExample formattingFunc element =
      [whamlet|
        <pre>
          #{formattingFunc element}
      |]
