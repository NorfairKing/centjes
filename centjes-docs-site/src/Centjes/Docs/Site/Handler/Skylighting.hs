{-# LANGUAGE OverloadedStrings #-}

module Centjes.Docs.Site.Handler.Skylighting (getSkylightingCssR) where

import Centjes.Docs.Site.Foundation
import qualified Skylighting

getSkylightingCssR :: Handler TypedContent
getSkylightingCssR = do
  neverExpires
  respond "text/css" $ Skylighting.styleToCss Skylighting.haddock
