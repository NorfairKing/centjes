{-# LANGUAGE LambdaCase #-}

module Centjes.Load where

import Centjes.Module
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Either
import qualified Data.Text.Encoding as TE
import Path
import System.Exit

-- TODO keep track of modules that we've already loaded so we can error on
-- cycles instead of looping infinitely
loadModules :: Path Abs File -> LoggingT IO Module
loadModules firstPath = do
  m <- readSingleModule firstPath
  Module <$> go firstPath m
  where
    go :: Path Abs File -> Module -> LoggingT IO [Declaration]
    go p m = do
      -- TODO separate the declarations into imports and non-imports
      -- so that we don't have to iterate over all non-import declarations
      -- This will require that we parse all imports first
      let (imports, decls) =
            partitionEithers $
              map
                ( \case
                    DeclarationImport i -> Left i
                    d -> Right d
                )
                (moduleDeclarations m)
      let d = parent p
      restDecls <- forM imports $ \(Import rf) -> do
        let af = d </> rf
        m' <- readSingleModule af
        go af m'
      pure $ concat (decls : restDecls)

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
        Right m -> pure m
