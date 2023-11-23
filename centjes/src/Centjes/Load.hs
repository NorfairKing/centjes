{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Load (loadModules) where

import Centjes.Module
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State.Strict
import qualified Data.ByteString as SB
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Error.Diagnose
import Path
import System.Exit

loadModules :: forall m. MonadLoggerIO m => Path Abs File -> m ([LDeclaration], Diagnostic String)
loadModules firstPath = do
  let base = parent firstPath
  (ds, visited) <- flip runStateT M.empty $ do
    m <- readSingle base firstPath
    go base base m
  pure (ds, foldl' (\d (f, c) -> addFile d (fromRelFile f) c) mempty (M.toList visited))
  where
    go :: Path Abs Dir -> Path Abs Dir -> LModule -> StateT (Map (Path Rel File) String) m [LDeclaration]
    go originalBase currentBase m = do
      restDecls <- forM (moduleImports m) $ \(Import rf) -> do
        let af = currentBase </> rf
        m' <- readSingle originalBase af
        go originalBase (parent af) m'
      pure $ concat (moduleDeclarations m : restDecls)

    readSingle :: Path Abs Dir -> Path Abs File -> StateT (Map (Path Rel File) String) m LModule
    readSingle originalBase p = do
      visited <- get
      case stripProperPrefix originalBase p of
        Nothing -> liftIO $ die "If you see this, there is a bug in the loadModules function of centjes."
        Just rf -> do
          if rf `M.member` visited
            then
              liftIO $
                die $
                  unlines
                    [ unwords ["Already loaded module at", fromAbsFile p],
                      "This means there is an import cycle."
                    ]
            else do
              (contents, m) <- lift $ readSingleModule originalBase rf
              modify' (M.insert rf contents)
              pure m

readSingleModule :: MonadLoggerIO m => Path Abs Dir -> Path Rel File -> m (String, LModule)
readSingleModule base p = do
  let fp = fromRelFile p
  contents <- liftIO $ SB.readFile $ fromAbsFile $ base </> p
  case TE.decodeUtf8' contents of
    Left e ->
      liftIO $
        die $
          unlines
            [ "Could not read file because it does not look like Utf-8: ",
              show fp,
              show e
            ]
    Right textContents -> do
      case parseModule fp textContents of
        Left e ->
          liftIO $
            die $
              unlines
                [ "Cannot parse file: ",
                  show fp,
                  e
                ]
        Right m -> do
          logDebugN $ T.pack $ unwords ["Read module:", fp]
          pure (T.unpack textContents, m)
