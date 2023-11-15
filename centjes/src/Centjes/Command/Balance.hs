{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Command.Balance (runCentjesBalance) where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Load
import Centjes.OptParse
import Centjes.Validation
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Map.Strict (Map)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import System.Exit
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout
import Text.Printf

runCentjesBalance :: Settings -> BalanceSettings -> IO ()
runCentjesBalance Settings {..} BalanceSettings = runStderrLoggingT $ do
  combinedModule <- loadModules settingLedgerFile
  liftIO $ case balanceLedger (compileModule combinedModule) of
    Failure errs -> die $ unlines $ "Balance failure:" : map renderBalanceError (NE.toList errs)
    Success accs -> do
      terminalCapabilities <- getTerminalCapabilitiesFromEnv
      let t = table (renderBalances accs)
      putChunksLocaleWith terminalCapabilities $ renderTable t

renderBalances :: Map AccountName Money.Account -> [[Chunk]]
renderBalances =
  map
    ( \(an, acc) ->
        [ accountNameChunk an,
          accountChunk acc
        ]
    )
    . M.toList

accountNameChunk :: AccountName -> Chunk
accountNameChunk = fore yellow . chunk . unAccountName

accountChunk :: Money.Account -> Chunk
accountChunk a =
  fore (if a >= Account.zero then green else red)
    . chunk
    . T.pack
    . printf "%20s"
    . show
    $ Account.toMinimalQuantisations a

balanceLedger :: Ledger -> Validation BalanceError (Map AccountName Money.Account)
balanceLedger m = do
  let incorporateAccounts :: Map AccountName Money.Account -> Map AccountName Money.Account -> Validation BalanceError (Map AccountName Money.Account)
      incorporateAccounts totals current =
        traverse
          ( \case
              Left (a1, a2) -> validationFailure $ BalanceErrorCouldNotAdd a1 a2
              Right a -> pure a
          )
          $ M.unionWith
            ( \ea1 ea2 -> do
                a1 <- ea1
                a2 <- ea2
                case Account.add a1 a2 of
                  Nothing -> Left (a1, a2)
                  Just a -> Right a
            )
            (M.map Right totals)
            (M.map Right current)
  mapM balanceTransaction (ledgerTransactions m) >>= foldM incorporateAccounts M.empty

balanceTransaction :: Transaction -> Validation BalanceError (Map AccountName Money.Account)
balanceTransaction t@Transaction {..} = do
  let incorporatePosting :: Map AccountName Money.Account -> Posting -> Validation BalanceError (Map AccountName Money.Account)
      incorporatePosting m (Posting an acc) = case M.lookup an m of
        Nothing -> pure $ M.insert an acc m
        Just acc' -> case Account.add acc acc' of
          Nothing -> validationFailure $ BalanceErrorCouldNotAdd acc acc'
          Just acc'' -> pure $ M.insert an acc'' m
  m <- foldM incorporatePosting M.empty transactionPostings
  let as = M.elems m
  case Account.sum as of
    Nothing -> validationFailure $ BalanceErrorCouldNotSum as
    Just d
      | d == Account.zero -> pure m
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance t d

data BalanceError
  = BalanceErrorCouldNotAdd !Money.Account !Money.Account
  | BalanceErrorCouldNotSum ![Money.Account]
  | BalanceErrorTransactionOffBalance !Transaction !Money.Account
  deriving stock (Show, Eq, Generic)

renderBalanceError :: BalanceError -> String
renderBalanceError = show
