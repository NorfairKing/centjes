module Centjes.Import.Revolut (runCentjesImportRevolut) where

import Centjes.Format
import Centjes.Module
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import System.Environment
import System.Exit

runCentjesImportRevolut :: IO ()
runCentjesImportRevolut = do
  args <- getArgs
  case args of
    [] -> die "Usage: centjes-import-revolut <file.csv>"
    (file : _) -> do
      print file
      let revolutModule = Module [] []
      SB.writeFile file $ TE.encodeUtf8 $ formatModule revolutModule
