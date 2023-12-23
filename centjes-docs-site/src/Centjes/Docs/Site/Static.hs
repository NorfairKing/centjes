{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Centjes.Docs.Site.Static
  ( module Centjes.Docs.Site.Static,
    DocPage (..),
  )
where

import Centjes.Docs.Site.Constants
import Centjes.Docs.Site.Static.TH
import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH.Load
import Path

docPages :: Load (Map [Text] DocPage)
docPages = $$(embedTextFilesInWith docPageKeyFunc [||docPageKeyFunc||] docPageValFunc [||docPageValFunc||] mode [reldir|content/pages|])
