{-# LANGUAGE TemplateHaskell #-}

module Centjes.Switzerland.Constants where

import Language.Haskell.TH
import Language.Haskell.TH.Load
import System.Environment

development :: Bool
development =
  $( do
       md <- runIO $ lookupEnv "DEVELOPMENT"
       fmap ConE $ case md of
         Nothing -> pure 'False
         Just _ -> do
           runIO $ putStrLn "WARNING: BUILDING centjes-switzerland IN DEVELOPMENT MODE"
           pure 'True
   )

mode :: Mode
mode = if development then LoadLive else BakeIn
