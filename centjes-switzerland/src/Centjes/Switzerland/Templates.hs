{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Switzerland.Templates where

import Centjes.Switzerland.Constants
import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH.Load
import Path

templateFileMap :: Load (Map (Path Rel File) Text)
templateFileMap = $$(embedTextFilesIn mode [reldir|templates|])
