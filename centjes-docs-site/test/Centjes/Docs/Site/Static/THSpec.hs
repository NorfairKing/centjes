{-# LANGUAGE OverloadedStrings #-}

module Centjes.Docs.Site.Static.THSpec (spec) where

import Centjes.Docs.Site.Static.TH
import qualified Data.Text as T
import Test.Syd

spec :: Spec
spec =
  describe "renderMarkdown" $
    it "renders a multi-line centjes block" $
      pureGoldenTextFile "test_resources/multi-line-centjes-block.html" $
        renderMarkdown $
          T.unlines
            [ "``` centjes",
              "account assets:bank",
              "  + attach statement.pdf",
              "```"
            ]
