{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Load
  ( loadModules,
    loadModules',
    loadModulesOrErr,
    diagFromFileMap,
  )
where

import Centjes.Location
import Centjes.Module
import Centjes.Parse
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State.Strict
import qualified Data.ByteString as SB
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Error.Diagnose
import GHC.Generics (Generic)
import Path
import Path.IO
import System.Exit

loadModules :: forall m. MonadLoggerIO m => Path Abs File -> m ([LDeclaration], Diagnostic String)
loadModules firstPath = do
  (declarations, fileMap) <- loadModules' firstPath
  pure (declarations, diagFromFileMap fileMap)

loadModules' :: forall m. MonadLoggerIO m => Path Abs File -> m ([LDeclaration], Map (Path Rel File) (Text, LModule))
loadModules' firstPath = do
  validation <- unValidationT $ loadModulesOrErr firstPath
  liftIO $ checkValidation mempty validation

data LoadError ann = LoadErrorImportMissing !(Path Rel File)
  deriving (Show, Eq, Generic)

instance ToReport (LoadError SourceSpan) where
  toReport = \case
    LoadErrorImportMissing rf ->
      Err
        (Just "LE_IMPORT")
        (unwords ["Imported module does not exist:", fromRelFile rf])
        []
        []

loadModulesOrErr :: forall m. MonadLoggerIO m => Path Abs File -> ValidationT (LoadError SourceSpan) m ([LDeclaration], Map (Path Rel File) (Text, LModule))
loadModulesOrErr firstPath = do
  let base = parent firstPath
  flip runStateT M.empty $ do
    m <- readSingle base firstPath
    go base base m
  where
    go ::
      Path Abs Dir ->
      Path Abs Dir ->
      LModule ->
      StateT
        (Map (Path Rel File) (Text, LModule))
        (ValidationT (LoadError ann) m)
        [LDeclaration]
    go originalBase currentBase m = do
      restDecls <- forM (moduleImports m) $ \(Import rf) -> do
        let af = currentBase </> rf
        m' <- readSingle originalBase af
        go originalBase (parent af) m'
      pure $ concat (moduleDeclarations m : restDecls)

    readSingle ::
      Path Abs Dir ->
      Path Abs File ->
      StateT
        (Map (Path Rel File) (Text, LModule))
        (ValidationT (LoadError ann) m)
        LModule
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
              modify' (M.insert rf (contents, m))
              pure m

diagFromFileMap :: Map (Path Rel File) (Text, LModule) -> Diagnostic String
diagFromFileMap =
  foldl'
    (\d (f, (c, _)) -> addFile d (fromRelFile f) (T.unpack c))
    mempty
    . M.toList

readSingleModule :: MonadLoggerIO m => Path Abs Dir -> Path Rel File -> ValidationT (LoadError ann) m (Text, LModule)
readSingleModule base p = do
  let fp = fromRelFile p
  let af = base </> p
  fileExist <- doesFileExist af
  if fileExist
    then do
      contents <- liftIO $ SB.readFile $ fromAbsFile af
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
          case parseModule base p textContents of
            Left e ->
              liftIO $
                die $
                  unlines
                    [ "Cannot parse file: ",
                      show fp,
                      e
                    ]
            Right m -> do
              lift $ logDebugN $ T.pack $ unwords ["Read module:", fp]
              pure (textContents, m)
    else validationTFailure $ LoadErrorImportMissing p
