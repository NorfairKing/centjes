{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Centjes.Switzerland.Templates
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import qualified Codec.Archive.Zip as Zip
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH.Load
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import qualified Money.MultiAccount as MultiAccount
import Path
import Path.IO
import System.Exit
import System.Process.Typed

runCentjesSwitzerland :: IO ()
runCentjesSwitzerland = do
  Instructions d settings <- getInstructions
  case d of
    DispatchTaxes taxesSettings -> runCentjesSwitzerlandTaxes settings taxesSettings

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
      validation <- liftIO $ runValidationT $ produceInputFromDeclarations settingSetup declarations
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

      -- Write the template to a file
      mainTypFile <- liftIO $ do
        mtf <- resolveFile tdir "main.typ"
        SB.writeFile (fromAbsFile mtf) $ TE.encodeUtf8 mainTypContents
        pure mtf

      -- Compile the README.pdf using typst
      runProcess_ $
        setWorkingDir (fromAbsDir (parent mainTypFile)) $
          setStdout inherit $
            setStderr inherit $
              proc
                "typst"
                [ "-v",
                  "compile",
                  fromAbsFile mainTypFile,
                  fromAbsFile taxesSettingReadmeFile,
                  "--root",
                  fromAbsDir tdir
                ]
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

data AnyError ann
  = AnyErrorCheck (CheckError ann)
  | AnyErrorInput (InputError ann)

instance ToReport (AnyError SourceSpan) where
  toReport = \case
    AnyErrorCheck ce -> toReport ce
    AnyErrorInput ie -> toReport ie

produceInputFromDeclarations ::
  Setup ->
  [Declaration SourceSpan] ->
  ValidationT (AnyError SourceSpan) IO (Input, Map (Path Rel File) (Path Rel File))
produceInputFromDeclarations setup declarations = do
  (ledger, balanceReport, _) <- mapValidationTFailure AnyErrorCheck $ doCompleteCheck declarations
  transformValidationT
    ( \f -> do
        (v, fs) <- runWriterT f
        pure $ (,) <$> v <*> pure fs
    )
    $ mapValidationTFailure AnyErrorInput
    $ produceInput setup ledger balanceReport

type P ann a = ValidationT (InputError ann) (WriterT (Map (Path Rel File) (Path Rel File)) IO) a

addEvidence :: Path Rel File -> Path Rel File -> P ann ()
addEvidence locationOnDisk locationInTarball = lift $ tell $ M.singleton locationOnDisk locationInTarball

produceInput ::
  Show ann =>
  Setup ->
  Ledger ann ->
  BalanceReport ann ->
  P ann Input
produceInput Setup {..} ledger (BalanceReport accountBalances) = do
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
          [Located _ (Attachment (Located _ path))] -> do
            let fileInTarball = [reldir|income|] </> path
            addEvidence path fileInTarball
            pure fileInTarball
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
    evidenceFile <- liftIO $ parseRelFile assetSetupEvidence
    let assetEvidence = [reldir|assets|] </> evidenceFile
    addEvidence evidenceFile assetEvidence
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
