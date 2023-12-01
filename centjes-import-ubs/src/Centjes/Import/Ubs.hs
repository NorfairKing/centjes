module Centjes.Import.Ubs (runCentjesImportUbs) where

import Centjes.Format
import Centjes.Module
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import System.Environment
import System.Exit

runCentjesImportUbs :: IO ()
runCentjesImportUbs = do
  args <- getArgs
  case args of
    [] -> die "Usage: centjes-import-ubs <file.csv>"
    (file : _) -> do
      print file
      let ubsModule = Module [] []
      SB.writeFile file $ TE.encodeUtf8 $ formatModule ubsModule
