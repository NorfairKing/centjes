module Centjes.Switzerland.Zip
  ( createZipFile,
  )
where

import qualified Codec.Archive.Zip as Zip
import Conduit
import Control.Monad.Writer
import qualified Data.ByteString as SB
import Data.Map (Map)
import qualified Data.Map as M
import Path

createZipFile ::
  Path Abs File ->
  -- Reverse mapping: To -> From
  -- Because the to's are unique but the from's aren't necessarily.
  Map (Path Rel File) (Path Abs File) ->
  IO ()
createZipFile zipFile fileMap =
  Zip.createArchive (fromAbsFile zipFile) $
    forM_ (M.toList fileMap) $ \(filePathTo, filePathFrom) -> do
      contents <- liftIO $ SB.readFile $ fromAbsFile filePathFrom
      selector <- Zip.mkEntrySelector $ fromRelFile filePathTo
      Zip.addEntry Zip.Deflate contents selector
      pure ()
