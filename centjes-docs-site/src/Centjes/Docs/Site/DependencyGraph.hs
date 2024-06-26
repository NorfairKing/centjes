{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Centjes.Docs.Site.DependencyGraph where

import Data.ByteString (ByteString)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment

dependencyGraph :: Maybe ByteString
dependencyGraph =
  $$( Code $ do
        md <- runIO $ lookupEnv "CENTJES_DOCS_DEPENDENCY_GRAPH"
        case md of
          Nothing -> do
            runIO $ putStrLn "WARNING: Building without dependency graph, set DEPENDENCY_GRAPH to build them during development."
            examineCode [||Nothing||]
          Just fp -> do
            runIO $ putStrLn $ "Building with dependency graph at " <> fp
            unsafeTExpCoerce (embedFileIfExists fp)
    )
