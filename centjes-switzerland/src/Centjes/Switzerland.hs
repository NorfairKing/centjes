{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland (runCentjesSwitzerland) where

import Centjes.AccountName (AccountName (..))
import Centjes.Command.Check
import Centjes.Load
import Centjes.Report.Balance
import Centjes.Switzerland.OptParse
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Money.MultiAccount as MultiAccount

runCentjesSwitzerland :: IO ()
runCentjesSwitzerland = do
  Settings {..} <- getSettings
  runStderrLoggingT $ do
    (declarations, diag) <- loadModules settingLedgerFile
    validation <- liftIO $ runValidationT $ doCompleteCheck declarations
    (ledger, balanceReport, register) <- liftIO $ checkValidation diag validation
    -- liftIO $ print ledger
    -- liftIO $ print balanceReport
    -- liftIO $ print register

    let finalBalances = unBalanceReport balanceReport
    liftIO $ print finalBalances
    let lookupBalance an = fromMaybe MultiAccount.zero (M.lookup an finalBalances)
    liftIO $ print $ lookupBalance $ AccountName "income"
    pure ()
