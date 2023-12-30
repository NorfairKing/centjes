{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland (runCentjesSwitzerland, produceInputFromDeclarations) where

import Autodocodec
import Centjes.Command.Check
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Module (Declaration)
import Centjes.Report.Balance
import Centjes.Switzerland.OptParse
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import qualified Money.MultiAccount as MultiAccount
import Path
import Path.IO
import System.Process.Typed

runCentjesSwitzerland :: IO ()
runCentjesSwitzerland = do
  Settings {..} <- getSettings
  runStderrLoggingT $ do
    (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
    validation <- liftIO $ runValidationT $ produceInputFromDeclarations settingBaseDir settingSetup declarations
    input <- liftIO $ checkValidation diag validation
    -- TODO Compile the templates into the binary
    mainTyp <- resolveFile' "templates/main.typ"
    outFile <- resolveFile' "example.pdf"
    liftIO $ compileTypstWithData input mainTyp outFile
    pure ()

data AnyError ann
  = AnyErrorCheck (CheckError ann)
  | AnyErrorInput (InputError ann)

instance ToReport (AnyError SourceSpan) where
  toReport = \case
    AnyErrorCheck ce -> toReport ce
    AnyErrorInput ie -> toReport ie

produceInputFromDeclarations :: Path Abs Dir -> Setup -> [Declaration SourceSpan] -> ValidationT (AnyError SourceSpan) IO Input
produceInputFromDeclarations setupDir setup declarations = do
  (ledger, balanceReport, _) <- mapValidationTFailure AnyErrorCheck $ doCompleteCheck declarations
  mapValidationTFailure AnyErrorInput $ produceInput setupDir setup ledger balanceReport

produceInput ::
  Show ann =>
  Path Abs Dir ->
  Setup ->
  Ledger ann ->
  BalanceReport ann ->
  ValidationT (InputError ann) IO Input
produceInput setupDir Setup {..} ledger (BalanceReport accountBalances) = do
  let inputName = setupName

  inputIncome <- flip V.mapMaybeM (ledgerTransactions ledger) $ \(Located _ Transaction {..}) -> do
    let Located _ timestamp = transactionTimestamp
    relevantAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
      let Located _ accountName = postingAccountName
      if S.member accountName setupIncomeAccounts
        then do
          let Located _ account = postingAccount
          let Located _ currency = postingCurrency
          amount <- case account of
            Negative amount -> pure $ amountToAmountWithCurrency currency amount
            Positive _ -> undefined -- TODO error
          pure $ Just amount
        else pure Nothing
    case V.toList relevantAccounts of
      [] -> pure Nothing
      [incomeAmount] -> do
        incomeEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> pure path
          _ -> undefined -- TODO error
        let incomeDay = Timestamp.toDay timestamp
        incomeDescription <- case transactionDescription of
          Nothing -> undefined -- TODO error
          Just (Located _ d) -> pure d
        pure $ Just Income {..}
      -- TODO Error
      _ -> undefined

  inputAssets <- fmap V.fromList $ for (M.toList setupAssetsAccounts) $ \(name, AssetSetup {..}) -> do
    let assetName = name
    -- TODO check that file exists
    evidenceFile <- liftIO $ resolveFile setupDir assetSetupEvidence
    -- TODO prefix the assets directory name
    let assetEvidence = filename evidenceFile
    assetAmount <- case M.lookup assetSetupAccountName accountBalances of
      Nothing -> undefined -- TODO error
      Just ma -> case M.toList (MultiAccount.unMultiAccount ma) of
        [] -> undefined -- TODO error
        [(currency, account)] -> case account of
          Positive amount -> pure $ amountToAmountWithCurrency currency amount
          _ -> undefined -- TODO error
        m -> error (show m) -- TODO error
    pure Asset {..}

  pure Input {..}

amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
amountToAmountWithCurrency currency amount =
  let Located _ qf = currencyQuantisationFactor currency
      symbol = currencySymbol currency
   in AmountWithCurrency (Amount.format qf amount) symbol

data InputError ann = InputError

instance ToReport (InputError SourceSpan) where
  toReport = \case
    InputError -> undefined

data Input = Input
  { inputName :: Text,
    inputIncome :: Vector Income,
    inputAssets :: Vector Asset
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "name" "name"
          .= inputName
        <*> requiredField "income" "income"
          .= inputIncome
        <*> requiredField "assets" "assets"
          .= inputAssets

data Income = Income
  { incomeDay :: !Day,
    incomeDescription :: !Description,
    incomeAmount :: !AmountWithCurrency,
    incomeEvidence :: !(Path Rel File)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Income)

instance HasCodec Income where
  codec =
    object "Income" $
      Income
        <$> requiredField "day" "day"
          .= incomeDay
        <*> requiredField "description" "description"
          .= incomeDescription
        <*> requiredField "amount" "amount"
          .= incomeAmount
        <*> requiredField "evidence" "evidence"
          .= incomeEvidence

data Asset = Asset
  { assetName :: !Text,
    assetAmount :: !AmountWithCurrency,
    assetEvidence :: !(Path Rel File)
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Asset)

instance HasCodec Asset where
  codec =
    object "Asset" $
      Asset
        <$> requiredField "name" "asset account name"
          .= assetName
        <*> requiredField "amount" "amount"
          .= assetAmount
        <*> requiredField "evidence" "evidence"
          .= assetEvidence

data AmountWithCurrency = AmountWithCurrency
  { amountWithCurrencyAmount :: String,
    amountWithCurrencyCurrency :: CurrencySymbol
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec AmountWithCurrency)

instance HasCodec AmountWithCurrency where
  codec =
    object "AmountWithCurrency" $
      AmountWithCurrency
        <$> requiredField "formatted" "formatted amount"
          .= amountWithCurrencyAmount
        <*> requiredField "symbol" "currency symbol"
          .= amountWithCurrencyCurrency

compileTypstWithData :: Input -> Path Abs File -> Path Abs File -> IO ()
compileTypstWithData input rootFile outputFile = do
  print rootFile
  print outputFile
  jsonInputFile <- resolveFile (parent rootFile) "input.json"
  SB.writeFile (fromAbsFile jsonInputFile) (LB.toStrict (JSON.encode input))
  runProcess_ $
    setWorkingDir (fromAbsDir (parent rootFile)) $
      proc
        "typst"
        [ "compile",
          fromAbsFile rootFile,
          fromAbsFile outputFile,
          "--root",
          fromAbsDir (parent rootFile)
        ]
