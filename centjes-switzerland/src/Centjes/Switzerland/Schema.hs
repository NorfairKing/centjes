{-# LANGUAGE TemplateHaskell #-}

module Centjes.Switzerland.Schema
  ( embeddedSchemas,
    withEmbeddedSchemaDir,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.FileEmbed (embedDir)
import Language.Haskell.TH.Syntax (runIO)
import Path
import Path.IO
import System.Environment (lookupEnv)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Temporary (withSystemTempDirectory)

-- | The XML schema files and their catalog, embedded at compile time.
--
-- The directory to embed is named by the @CENTJES_SWITZERLAND_SCHEMA_DIR@
-- environment variable, which the Nix build and the development shell both set
-- to a directory assembled from upstream (see @nix/schemas.nix@). This avoids
-- vendoring the schemas in the repository while keeping the executable
-- self-contained: the runtime @xmllint@ validation needs the files on disk.
embeddedSchemas :: [(FilePath, ByteString)]
embeddedSchemas =
  $( do
       mSchemaDir <- runIO $ lookupEnv "CENTJES_SWITZERLAND_SCHEMA_DIR"
       case mSchemaDir of
         Nothing -> do
           runIO $ putStrLn "WARNING: Building without XML schemas, set CENTJES_SWITZERLAND_SCHEMA_DIR to validate output during development."
           [|[]|]
         Just schemaDir -> do
           runIO $ putStrLn $ "Building with XML schemas at " <> schemaDir
           embedDir schemaDir
   )

-- | Write the 'embeddedSchemas' to a temporary directory and run the action
-- with its path.
--
-- @xmllint@ needs the schemas and their catalog as real files on disk, so we
-- materialise the embedded contents for the duration of the action.
withEmbeddedSchemaDir :: (MonadUnliftIO m) => (Path Abs Dir -> m a) -> m a
withEmbeddedSchemaDir action =
  withSystemTempDirectory "centjes-switzerland-schemas" $ \tmpFilePath -> do
    tmpDir <- liftIO $ parseAbsDir tmpFilePath
    liftIO $
      forM_ embeddedSchemas $ \(relFilePath, contents) -> do
        relFile <- parseRelFile relFilePath
        let destination = tmpDir </> relFile
        ensureDir (parent destination)
        SB.writeFile (fromAbsFile destination) contents
    action tmpDir
