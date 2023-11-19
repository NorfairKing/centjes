{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Check (runCentjesCheck) where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Load
import Centjes.Module
import Centjes.OptParse
import Centjes.Validation
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import Money.QuantisationFactor
import System.Exit
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout
import Text.Printf

runCentjesCheck :: Settings -> CheckSettings -> IO ()
runCentjesCheck Settings {..} CheckSettings = runStderrLoggingT $ do
  declarations <- loadModules settingLedgerFile
  pure ()

data CheckError = CheckError
  deriving (Show, Eq, Generic)

instance Exception CheckError where
  displayException = \case
    CheckError -> ""

checkDeclarations :: [Declaration] -> Validation CheckError ()
checkDeclarations ds = pure ()
