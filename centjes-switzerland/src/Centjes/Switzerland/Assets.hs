{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Switzerland.Assets where

import Centjes.Switzerland.Constants
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Language.Haskell.TH.Load
import Path
import System.Exit

requireAsset :: Path Rel File -> IO Text
requireAsset rf = do
  assetMap <- loadIO assetFileMap
  case M.lookup rf assetMap of
    Nothing -> die $ unwords ["Asset not found:", fromRelFile rf]
    Just contents -> pure contents

assetFileMap :: Load (Map (Path Rel File) Text)
assetFileMap = $$(embedTextFilesIn mode [reldir|assets|])
