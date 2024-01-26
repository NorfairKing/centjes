{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Switzerland.Assets where

import Centjes.Switzerland.Constants
import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH.Load
import Path

assetFileMap :: Load (Map (Path Rel File) Text)
assetFileMap = $$(embedTextFilesIn mode [reldir|assets|])
