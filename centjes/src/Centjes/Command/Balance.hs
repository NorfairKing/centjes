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
import Control.Exception
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
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
import System.Exit
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout
import Text.Printf

runCentjesBalance :: Settings -> BalanceSettings -> IO ()
runCentjesBalance Settings {..} BalanceSettings = runStderrLoggingT $ do
  declarations <- loadModules settingLedgerFile
  liftIO $ case compileDeclarations declarations of
    Failure errs -> die $ unlines $ "Compilation failure: " : map displayException (NE.toList errs)
    Success ledger ->
      case balanceLedger ledger of
        Failure errs -> die $ unlines $ "Balance failure:" : map renderBalanceError (NE.toList errs)
        Success accs -> do
          terminalCapabilities <- getTerminalCapabilitiesFromEnv
          let t = table (renderBalances accs)
          putChunksLocaleWith terminalCapabilities $ renderTable t

renderBalances :: Map AccountName (Money.MultiAccount CurrencySymbol) -> [[Chunk]]
renderBalances =
  concatMap
    ( \(an, acc) ->
        case multiAccountChunks acc of
          [] -> []
          (firstChunks : rest) ->
            (accountNameChunk an : firstChunks)
              : map (chunk " " :) rest
    )
    . M.toList

accountNameChunk :: AccountName -> Chunk
accountNameChunk = fore yellow . chunk . unAccountName

multiAccountChunks :: Money.MultiAccount CurrencySymbol -> [[Chunk]]
multiAccountChunks ma =
  let accounts = MultiAccount.unMultiAccount ma
   in map (\(cs, acc) -> [accountChunk acc, currencySymbolChunk cs]) (M.toList accounts)

currencySymbolChunk :: CurrencySymbol -> Chunk
currencySymbolChunk = fore blue . chunk . unCurrencySymbol

accountChunk :: Money.Account -> Chunk
accountChunk a =
  fore (if a >= Account.zero then green else red)
    . chunk
    . T.pack
    . printf "%20s"
    . show
    $ Account.toMinimalQuantisations a

balanceLedger :: Ledger -> Validation BalanceError (Map AccountName (Money.MultiAccount CurrencySymbol))
balanceLedger m = do
  let incorporateAccounts ::
        Map AccountName (Money.MultiAccount CurrencySymbol) ->
        Map AccountName (Money.MultiAccount CurrencySymbol) ->
        Validation BalanceError (Map AccountName (Money.MultiAccount CurrencySymbol))
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
                case MultiAccount.add a1 a2 of
                  Nothing -> Left (a1, a2)
                  Just a -> Right a
            )
            (M.map Right totals)
            (M.map Right current)
  mapM balanceTransaction (ledgerTransactions m) >>= foldM incorporateAccounts M.empty

balanceTransaction :: Transaction -> Validation BalanceError (Map AccountName (Money.MultiAccount CurrencySymbol))
balanceTransaction t@Transaction {..} = do
  let incorporatePosting ::
        Map AccountName (Money.MultiAccount CurrencySymbol) ->
        Posting ->
        Validation BalanceError (Map AccountName (Money.MultiAccount CurrencySymbol))
      incorporatePosting m (Posting an currency account) =
        let acc = MultiAccount.fromAccount (currencySymbol currency) account
         in case M.lookup an m of
              Nothing -> pure $ M.insert an acc m
              Just acc' -> case MultiAccount.add acc acc' of
                Nothing -> validationFailure $ BalanceErrorCouldNotAdd acc acc'
                Just acc'' -> pure $ M.insert an acc'' m
  m <- foldM incorporatePosting M.empty transactionPostings
  let as = M.elems m
  case MultiAccount.sum as of
    Nothing -> validationFailure $ BalanceErrorCouldNotSum as
    Just d
      | d == MultiAccount.zero -> pure m
      | otherwise -> validationFailure $ BalanceErrorTransactionOffBalance t d

data BalanceError
  = BalanceErrorCouldNotAdd !(Money.MultiAccount CurrencySymbol) !(Money.MultiAccount CurrencySymbol)
  | BalanceErrorCouldNotSum ![Money.MultiAccount CurrencySymbol]
  | BalanceErrorTransactionOffBalance !Transaction !(Money.MultiAccount CurrencySymbol)
  deriving stock (Show, Eq, Generic)

renderBalanceError :: BalanceError -> String
renderBalanceError = show
