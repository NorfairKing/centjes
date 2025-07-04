{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Load
  ( loadMWatchedModules,
    loadWatchedModules,
    loadModules,
    loadModules',
    loadModulesOrErr,
    LoadError (..),
    LoadError',
    diagFromFileMap,
    diagFromFileMap',
  )
where

import Centjes.Location
import Centjes.Module
import Centjes.Parse
import Centjes.Timing
import Centjes.Validation
import Control.Monad
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.State.Strict
import qualified Data.ByteString as SB
import Data.Foldable
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Error.Diagnose
import Path
import Path.IO
import System.Exit
import System.FSNotify as Notify
import System.IO
import Text.Read (readMaybe)
import UnliftIO

loadMWatchedModules ::
  (MonadUnliftIO m, MonadLogger m) =>
  Bool ->
  Path Abs File ->
  (([LDeclaration], Map (Path Rel File) (Text, LModule)) -> m ()) ->
  m ()
loadMWatchedModules watch firstPath func = do
  -- Set locale encoding so diagnose can output unicode characters.
  liftIO $ do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
  if watch
    then loadWatchedModules firstPath func
    else loadModules' firstPath >>= func

loadWatchedModules ::
  forall m.
  (MonadUnliftIO m, MonadLogger m) =>
  Path Abs File ->
  (([LDeclaration], Map (Path Rel File) (Text, LModule)) -> m ()) ->
  m ()
loadWatchedModules firstPath func = liftWith Notify.withManager $ \watchManager -> do
  let dir = parent firstPath
  let loop :: Maybe (Map (Path Rel Dir) (Set (Path Rel File))) -> m ()
      loop mDirMap = do
        logDebugN "Clearing"
        liftIO clearScreen
        eventChan <- newChan
        errOrTup <- (Right <$> loadModules' firstPath) `catch` (\ec -> pure (Left (ec :: ExitCode)))
        let dirMap :: Map (Path Rel Dir) (Set (Path Rel File))
            dirMap = case errOrTup of
              Left _ -> case mDirMap of
                Nothing -> M.singleton [reldir|.|] (S.singleton (filename firstPath))
                Just dirMap' -> dirMap'
              Right (_, fileMap) ->
                M.unionsWith S.union $
                  map (\f -> M.singleton (parent f) (S.singleton (filename f))) $
                    M.keys fileMap

        let subdirPredicate :: Path Rel Dir -> Set (Path Rel File) -> Event -> Bool
            subdirPredicate subdir subFileSet e =
              let isRelevantFile = isJust $ do
                    absFile <- parseAbsFile $ eventPath e
                    relFile <- stripProperPrefix (dir </> subdir) absFile
                    guard $ S.member relFile subFileSet
                  isRelevantEvent = case e of
                    Added {} -> True
                    Modified {} -> True
                    ModifiedAttributes {} -> False
                    Removed {} ->
                      -- We don't want to run a report when a file is deleted and added in quick succession
                      False
                    WatchedDirectoryRemoved {} -> False
                    CloseWrite {} -> False
                    Unknown {} -> False
               in and [isRelevantFile, eventIsDirectory e == IsFile, isRelevantEvent]

        let watchSubdir :: Path Rel Dir -> Set (Path Rel File) -> m StopListening
            watchSubdir subdir subFileSet = do
              logDebugN $ T.pack $ unwords ["Watching", fromRelDir subdir]
              liftIO $ watchDirChan watchManager (fromAbsDir (dir </> subdir)) (subdirPredicate subdir subFileSet) eventChan

        let watchDirMap :: m [StopListening]
            watchDirMap = mapM (uncurry watchSubdir) $ M.toList dirMap

        bracket watchDirMap (liftIO . sequence_) $ \_ -> do
          case errOrTup of
            Left _ -> do
              logDebugN "Error loading modules"
              pure ()
            Right (declarations, fileMap) -> do
              -- Run the function with the loaded modules
              func (declarations, fileMap)
                `catch` ( \case
                            ExitSuccess -> pure ()
                            ExitFailure _ -> pure ()
                        )
          -- Wait for an event
          event <- readChan eventChan
          logDebugN $ T.pack $ unwords ["Changed:", maybe "a file" fromRelFile $ parseAbsFile (eventPath event) >>= stripProperPrefix dir]
          pure ()
        loop (Just dirMap)

  loop Nothing

-- Clear screen using ANSI codes
clearScreen :: IO ()
clearScreen = hPutStr stdout "\ESCc" >> hPutStr stderr "\ESCc"

liftWith ::
  (MonadUnliftIO m) =>
  ((x -> IO a) -> IO a) ->
  ((x -> m a) -> m a)
liftWith withFunc func = do
  unlift <- askRunInIO
  liftIO $ withFunc $ \x -> unlift (func x)

loadModules :: forall m. (MonadIO m, MonadLogger m) => Path Abs File -> m ([LDeclaration], Diagnostic String)
loadModules firstPath = do
  (declarations, fileMap) <- withLoggedDuration "Load" $ loadModules' firstPath
  pure (declarations, diagFromFileMap fileMap)

loadModules' :: forall m. (MonadIO m, MonadLogger m) => Path Abs File -> m ([LDeclaration], Map (Path Rel File) (Text, LModule))
loadModules' firstPath = do
  errOrRes <- runExceptT $ loadModulesOrErr firstPath
  case errOrRes of
    Right a -> pure a
    Left (LoadError fileMap le) -> do
      let diag = addReport (diagFromFileMap' fileMap) (toReport le)
      liftIO $ dieWithDiag diag

data LoadError = LoadError !(Map (Path Rel File) Text) !LoadError'

data LoadError'
  = LoadErrorImportMissing !(Path Rel File) !(Maybe SourceSpan)
  | LoadErrorNotAFile !(Path Rel File) !(Maybe SourceSpan)
  | LoadErrorNotUtf8 !(Path Rel File) !(Maybe SourceSpan)
  | LoadErrorParseError !(Path Rel File) !(Maybe SourceSpan) !String

instance ToReport LoadError' where
  toReport = \case
    LoadErrorImportMissing rf mL ->
      Err
        (Just "LE_IMPORT_MISSING")
        (unwords ["Imported module does not exist:", fromRelFile rf])
        [(toDiagnosePosition l, This "Imported here") | l <- maybeToList mL]
        []
    LoadErrorNotAFile rf mL ->
      Err
        (Just "LE_IMPORT_FILE")
        (unwords ["Imported module is not a file:", fromRelFile rf])
        [(toDiagnosePosition l, This "Imported here") | l <- maybeToList mL]
        []
    LoadErrorNotUtf8 rf mL ->
      Err
        (Just "LE_UTF8")
        (unwords ["Imported module is not utf-8 encoded:", fromRelFile rf])
        [(toDiagnosePosition l, This "Imported here") | l <- maybeToList mL]
        []
    LoadErrorParseError rf mL e ->
      Err
        (Just "LE_UTF8")
        ( case maybePos of
            Just _ -> unwords ["Could not parse:", fromRelFile rf]
            Nothing ->
              intercalate
                "\n"
                [ unwords ["Could not parse:", fromRelFile rf],
                  e
                ]
        )
        ( concat
            [ [(toDiagnosePosition l, This "Imported here") | l <- maybeToList mL],
              -- Maybe we don't need to unpack and repack here. Then we can also have columns in filenames.
              [ ( Position beginTup endTup (fromRelFile rf),
                  This (T.unpack errPart)
                )
                | (errPart, beginTup, endTup) <- maybeToList maybePos
              ]
            ]
        )
        []
      where
        maybePos = case T.splitOn "  " (T.pack e) of
          diagPart : errPart : _ -> case T.splitOn "@" diagPart of
            _file : rest : _ -> case T.splitOn "-" rest of
              beginStr : endStr : _ ->
                let readPos s = case T.splitOn ":" s of
                      lineStr : colStr : _ ->
                        (,) <$> readMaybe (T.unpack lineStr) <*> readMaybe (T.unpack colStr)
                      _ -> Nothing
                 in (,,) errPart <$> readPos beginStr <*> readPos endStr
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing

loadModulesOrErr ::
  forall m.
  (MonadIO m, MonadLogger m) =>
  Path Abs File ->
  ExceptT LoadError m ([LDeclaration], Map (Path Rel File) (Text, LModule))
loadModulesOrErr firstPath = do
  let base = parent firstPath
  flip runStateT M.empty $ do
    m <- readSingle base Nothing firstPath
    go base base m
  where
    go ::
      Path Abs Dir ->
      Path Abs Dir ->
      LModule ->
      StateT
        (Map (Path Rel File) (Text, LModule))
        (ExceptT LoadError m)
        [LDeclaration]
    go originalBase currentBase m = do
      restDecls <- forM (moduleImports m) $ \(Located il (Import (Located _ mn))) -> do
        rf <- liftIO $ replaceExtension ".cent" mn
        let af = currentBase </> rf
        m' <- readSingle originalBase (Just il) af
        go originalBase (parent af) m'
      pure $ concat (moduleDeclarations m : restDecls)

    readSingle ::
      Path Abs Dir ->
      Maybe SourceSpan ->
      Path Abs File ->
      StateT
        (Map (Path Rel File) (Text, LModule))
        (ExceptT LoadError m)
        LModule
    readSingle originalBase mIl p = do
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
              fileMap <- get
              (contents, m) <- lift $ readSingleModule originalBase (M.map fst fileMap) mIl rf
              modify' (M.insert rf (contents, m))
              pure m

diagFromFileMap :: Map (Path Rel File) (Text, LModule) -> Diagnostic String
diagFromFileMap = diagFromFileMap' . M.map fst

diagFromFileMap' :: Map (Path Rel File) Text -> Diagnostic String
diagFromFileMap' =
  foldl'
    (\d (f, c) -> addFile d (fromRelFile f) (T.unpack c))
    mempty
    . M.toList

readSingleModule ::
  (MonadIO m, MonadLogger m) =>
  Path Abs Dir ->
  Map (Path Rel File) Text ->
  Maybe SourceSpan ->
  Path Rel File ->
  ExceptT LoadError m (Text, LModule)
readSingleModule base fileMap mIl p = do
  let fp = fromRelFile p
  let af = base </> p
  let loadError = throwError . LoadError fileMap
  occupied <- isLocationOccupied af
  if occupied
    then do
      isFile <- doesFileExist af
      if isFile
        then do
          contents <- liftIO $ SB.readFile $ fromAbsFile af
          case TE.decodeUtf8' contents of
            Left _ -> loadError $ LoadErrorNotUtf8 p mIl
            Right textContents -> do
              case parseModule base p textContents of
                Left e -> throwError $ LoadError (M.insert p textContents fileMap) $ LoadErrorParseError p mIl e
                Right m -> do
                  lift $ logDebugN $ T.pack $ unwords ["Read module:", fp]
                  pure (textContents, m)
        else throwError $ LoadError fileMap $ LoadErrorNotAFile p mIl
    else throwError $ LoadError fileMap $ LoadErrorImportMissing p mIl
