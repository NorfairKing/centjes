{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Balance (runCentjesBalance) where

import Centjes.Module
import Centjes.OptParse
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Path
import System.Exit

runCentjesBalance :: Settings -> BalanceSettings -> IO ()
runCentjesBalance Settings {..} BalanceSettings = runStderrLoggingT $ do
  let fp = settingLedgerFile
  mainModule <- do
    contents <- liftIO $ SB.readFile (fromAbsFile fp)
    case TE.decodeUtf8' contents of
      Left err ->
        liftIO $
          die $
            unlines
              [ "Could not read file because it does not look like Utf-8: ",
                show fp,
                show err
              ]
      Right textContents -> do
        case parseModule (fromAbsFile fp) textContents of
          Left err ->
            liftIO $
              die $
                unlines
                  [ "Cannot parse file: ",
                    show fp,
                    err
                  ]
          Right m -> pure m
  liftIO $ forM_ (moduleDeclarations mainModule) $ \(DeclarationTransaction transaction) -> do
    print (balanceTransaction transaction)

data Balancing
  = Balanced
  | OffBalanceBy !Money.Account
  | CouldNotBalance
  deriving stock (Show, Eq, Generic)

balanceTransaction :: Transaction -> Balancing
balanceTransaction Transaction {..} =
  let actualSum = Account.sum (map postingAmount transactionPostings)
   in case actualSum of
        Nothing -> CouldNotBalance
        Just s ->
          if s == Account.zero
            then Balanced
            else OffBalanceBy s
