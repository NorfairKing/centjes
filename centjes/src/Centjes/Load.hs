module Centjes.Load where

import Centjes.Module
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import System.Exit

-- TODO keep track of modules that we've already loaded so we can error on
-- cycles instead of looping infinitely
loadModules :: Path Abs File -> LoggingT IO [Declaration]
loadModules firstPath = do
  m <- readSingleModule firstPath
  go firstPath m
  where
    go :: Path Abs File -> Module -> LoggingT IO [Declaration]
    go p m = do
      let d = parent p
      restDecls <- forM (moduleImports m) $ \(Import rf) -> do
        let af = d </> rf
        m' <- readSingleModule af
        go af m'
      pure $ concat (moduleDeclarations m : restDecls)

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
