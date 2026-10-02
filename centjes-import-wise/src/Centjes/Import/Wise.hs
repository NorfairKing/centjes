{-# LANGUAGE RecordWildCards #-}

module Centjes.Import.Wise (runCentjesImportWise) where

import Centjes.Compile
import Centjes.Format
import Centjes.Import.Wise.Declarations
import Centjes.Import.Wise.OptParse
import Centjes.Import.Wise.Pair
import Centjes.Import.Wise.Statement
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Validation
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import Path
import System.Exit

runCentjesImportWise :: IO ()
runCentjesImportWise = do
  settings@Settings {..} <- getSettings
  (declarations, diag) <- runStderrLoggingT $ loadModules settingLedgerFile
  currencies <- checkValidation diag $ compileDeclarationsCurrencies declarations

  rows <- concat <$> traverse readStatementFile (NE.toList settingInputs)
  events <- case pairRows rows of
    Left errs -> die $ unlines $ map renderPairError errs
    Right events -> pure events
  transactions <-
    checkValidation diag $
      wiseTransactions (declarationSettings settings) currencies events

  let m =
        Module
          { moduleImports = [],
            moduleDeclarations = map (noLoc . DeclarationTransaction . noLoc) transactions
          }
  SB.writeFile (fromAbsFile settingOutput) (TE.encodeUtf8 (formatModule m))

readStatementFile :: Path Abs File -> IO [Row]
readStatementFile statementFile = do
  contents <- SB.readFile (fromAbsFile statementFile)
  case decodeStatement contents of
    Left err -> die $ unlines [unwords ["Could not read", fromAbsFile statementFile <> ":"], err]
    Right rows -> pure rows
