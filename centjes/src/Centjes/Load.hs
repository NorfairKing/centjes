module Centjes.Load where

import Centjes.Module
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State.Strict
import qualified Data.ByteString as SB
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import System.Exit

loadModules :: Path Abs File -> LoggingT IO [Declaration]
loadModules firstPath = do
  flip evalStateT S.empty $ do
    m <- readSingle firstPath
    go firstPath m
  where
    go :: Path Abs File -> Module -> StateT (Set (Path Abs File)) (LoggingT IO) [Declaration]
    go p m = do
      let d = parent p
      restDecls <- forM (moduleImports m) $ \(Import rf) -> do
        let af = d </> rf
        m' <- readSingle af
        go af m'
      pure $ concat (moduleDeclarations m : restDecls)

    readSingle :: Path Abs File -> StateT (Set (Path Abs File)) (LoggingT IO) Module
    readSingle p = do
      visited <- get
      if p `S.member` visited
        then
          liftIO $
            die $
              unlines
                [ unwords ["Already loaded module at", fromAbsFile p],
                  "This means there is an import cycle."
                ]
        else do
          modify' (S.insert p)
          lift $ readSingleModule p

readSingleModule :: Path Abs File -> LoggingT IO Module
readSingleModule fp = do
  contents <- liftIO $ SB.readFile (fromAbsFile fp)
  case TE.decodeUtf8' contents of
    Left err ->
      liftIO $
        die $
          unlines
            [ "Could not read file because it does not look like Utf-8: ",
              show fp,
              show err
            ]
    Right textContents -> do
      case parseModule (fromAbsFile fp) textContents of
        Left err ->
          liftIO $
            die $
              unlines
                [ "Cannot parse file: ",
                  show fp,
                  err
                ]
        Right m -> do
          logDebugN $ T.pack $ unwords ["Read module:", fromAbsFile fp]
          pure m
