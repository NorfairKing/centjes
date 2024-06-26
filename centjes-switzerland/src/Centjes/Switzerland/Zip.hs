module Centjes.Switzerland.Zip
  ( createZipFile,
  )
where

import qualified Codec.Archive.Zip as Zip
import Conduit
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Path

createZipFile ::
  (MonadLoggerIO m) =>
  Path Abs File ->
  -- Reverse mapping: To -> From
  -- Because the to's are unique but the from's aren't necessarily.
  Map (Path Rel File) (Path Abs File) ->
  m ()
createZipFile zipFile fileMap = do
  forM_ (M.toList fileMap) $ \(filePathTo, filePathFrom) ->
    logInfoN $ T.pack $ unwords ["Including", fromAbsFile filePathFrom, "at", fromRelFile filePathTo]

  Zip.createArchive (fromAbsFile zipFile) $
    forM_ (M.toList fileMap) $ \(filePathTo, filePathFrom) -> do
      contents <- liftIO $ SB.readFile $ fromAbsFile filePathFrom
      selector <- Zip.mkEntrySelector $ fromRelFile filePathTo
      Zip.addEntry Zip.Deflate contents selector
      pure ()
