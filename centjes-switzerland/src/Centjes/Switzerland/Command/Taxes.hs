{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Command.Taxes
  ( runCentjesSwitzerlandTaxes,
  )
where

import Centjes.Load
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.Taxes
import Centjes.Switzerland.Templates
import Centjes.Switzerland.Typst
import Centjes.Validation
import qualified Codec.Archive.Zip as Zip
import Conduit
import Control.Monad.Logger
import Control.Monad.Writer
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH.Load
import Path
import Path.IO
import System.Exit

runCentjesSwitzerlandTaxes :: Settings -> TaxesSettings -> IO ()
runCentjesSwitzerlandTaxes Settings {..} TaxesSettings {..} = do
  templatesMap <- loadIO templateFileMap
  mainTypContents <- case M.lookup [relfile|main.typ|] templatesMap of
    Nothing -> die "main.typ template not found."
    Just t -> pure t
  withSystemTempDir "centjes-switzerland" $ \tdir -> do
    runStderrLoggingT $ do
      -- Produce the input.json structure
      (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
      validation <- liftIO $ runValidationT $ produceTaxesInputFromDeclarations settingSetup declarations
      (input, files) <- liftIO $ checkValidation diag validation

      -- Write the input to a file
      jsonInputFile <- liftIO $ do
        jif <- resolveFile tdir "input.json"
        SB.writeFile (fromAbsFile jif) (LB.toStrict (JSON.encode input))
        pure jif
      logInfoN $
        T.pack $
          unwords
            [ "Succesfully compiled information into",
              fromAbsFile jsonInputFile
            ]
      logDebugN $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty input

      -- Write the template to a file
      mainTypFile <- liftIO $ do
        mtf <- resolveFile tdir "main.typ"
        SB.writeFile (fromAbsFile mtf) $ TE.encodeUtf8 mainTypContents
        pure mtf

      -- Compile the README.pdf using typst
      liftIO $ compileTypst mainTypFile taxesSettingReadmeFile

      logInfoN $
        T.pack $
          unwords
            [ "Typst compilation succesfully created",
              fromAbsFile taxesSettingReadmeFile
            ]

      let allFilesToInclude =
            (taxesSettingReadmeFile, [relfile|README.pdf|])
              : (jsonInputFile, [relfile|raw-input.json|])
              : [(settingBaseDir </> fromRel, to) | (fromRel, to) <- M.toList files]

      -- Create a nice zip file
      liftIO $
        Zip.createArchive (fromAbsFile taxesSettingZipFile) $
          forM_ allFilesToInclude $ \(filePathFrom, filePathTo) -> do
            contents <- liftIO $ SB.readFile $ fromAbsFile filePathFrom
            selector <- Zip.mkEntrySelector $ fromRelFile filePathTo
            Zip.addEntry Zip.Deflate contents selector
            pure ()
      logInfoN $
        T.pack $
          unwords
            [ "Succesfully created packet",
              fromAbsFile taxesSettingZipFile
            ]
