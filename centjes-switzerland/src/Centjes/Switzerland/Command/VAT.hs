{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Command.VAT
  ( runCentjesSwitzerlandVAT,
    produceVATInputFromDeclarations,
  )
where

import Autodocodec
import Centjes.Command.Check
import Centjes.Convert
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
import Conduit
import Control.Monad.Logger
import Control.Monad.Writer
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Time.Calendar.Quarter
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH.Load
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import qualified Money.MultiAccount as MultiAccount
import Path
import Path.IO
import System.Exit
import System.Process.Typed

runCentjesSwitzerlandVAT :: Settings -> VATSettings -> IO ()
runCentjesSwitzerlandVAT Settings {..} VATSettings {..} = do
  templatesMap <- loadIO templateFileMap
  mainTypContents <- case M.lookup [relfile|vat.typ|] templatesMap of
    Nothing -> die "vat.typ template not found."
    Just t -> pure t
  withSystemTempDir "centjes-switzerland" $ \tdir -> do
    runStderrLoggingT $ do
      -- Produce the input.json structure
      (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
      validation <- liftIO $ runValidationT $ produceVATInputFromDeclarations settingSetup declarations
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
      logDebugN $ TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty input

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
                  fromAbsFile vatSettingReadmeFile,
                  "--root",
                  fromAbsDir tdir
                ]
      logInfoN $
        T.pack $
          unwords
            [ "Typst compilation succesfully created",
              fromAbsFile vatSettingReadmeFile
            ]

      let allFilesToInclude =
            (vatSettingReadmeFile, [relfile|README.pdf|])
              : (jsonInputFile, [relfile|raw-input.json|])
              : [(settingBaseDir </> fromRel, to) | (fromRel, to) <- M.toList files]

      -- Create a nice zip file
      liftIO $
        Zip.createArchive (fromAbsFile vatSettingZipFile) $
          forM_ allFilesToInclude $ \(filePathFrom, filePathTo) -> do
            contents <- liftIO $ SB.readFile $ fromAbsFile filePathFrom
            selector <- Zip.mkEntrySelector $ fromRelFile filePathTo
            Zip.addEntry Zip.Deflate contents selector
            pure ()
      logInfoN $
        T.pack $
          unwords
            [ "Succesfully created packet",
              fromAbsFile vatSettingZipFile
            ]

data AnyError ann
  = AnyErrorCheck (CheckError ann)
  | AnyErrorInput (InputError ann)

instance ToReport (AnyError SourceSpan) where
  toReport = \case
    AnyErrorCheck ce -> toReport ce
    AnyErrorInput ie -> toReport ie

produceVATInputFromDeclarations ::
  Setup ->
  [Declaration SourceSpan] ->
  ValidationT (AnyError SourceSpan) IO (Input, Map (Path Rel File) (Path Rel File))
produceVATInputFromDeclarations setup declarations = do
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
  (Show ann, Ord ann) =>
  Setup ->
  Ledger ann ->
  BalanceReport ann ->
  P ann Input
produceInput Setup {..} ledger BalanceReport {..} = do
  let inputName = setupName
  let inputQuarter = fromMaybe (YearQuarter 2024 Q1) setupVATQuarter

  let chfSymbol = CurrencySymbol "CHF"
  chfCurrency <- case M.lookup chfSymbol (ledgerCurrencies ledger) of
    Nothing -> undefined
    Just lqf -> pure $ Currency {currencySymbol = chfSymbol, currencyQuantisationFactor = lqf}

  let priceGraph = pricesToPriceGraph (ledgerPrices ledger)

  inputIncome <- flip V.mapMaybeM (ledgerTransactions ledger) $ \(Located _ Transaction {..}) -> do
    let Located _ timestamp = transactionTimestamp
    let day = Timestamp.toDay timestamp
    if periodFirstDay inputQuarter <= day && day <= periodLastDay inputQuarter
      then do
        let incomeDay = Timestamp.toDay timestamp
        incomeDescription <- case transactionDescription of
          Nothing -> undefined -- TODO error
          Just (Located _ d) -> pure d
        incomeEvidence <- case V.toList transactionAttachments of
          [] -> undefined -- TODO error
          [Located _ (Attachment (Located _ path))] -> do
            let fileInTarball = [reldir|income|] </> path
            addEvidence path fileInTarball
            pure fileInTarball
          _ -> undefined -- TODO error
        relevantAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
          let Located _ accountName = postingAccountName
          if M.member accountName setupIncomeAccounts
            then do
              let Located _ account = postingAccount
              let Located _ currency = postingCurrency
              case account of
                Negative amount -> pure $ Just (amount, currency)
                Positive _ -> undefined -- TODO error
            else pure Nothing
        relevantVATAccounts <- flip V.mapMaybeM transactionPostings $ \(Located _ Posting {..}) -> do
          let Located _ accountName = postingAccountName
          if accountName == setupVATIncomeAccount
            then do
              let Located _ account = postingAccount
              let Located _ currency = postingCurrency
              -- TODO check that the currency is CHF
              case account of
                Negative amount -> pure $ Just $ amountToAmountWithCurrency currency amount
                Positive _ -> undefined -- TODO error
            else pure Nothing
        case V.toList relevantAccounts of
          [] -> pure Nothing
          [(amount, currency)] -> do
            let incomeAmount = amountToAmountWithCurrency currency amount
            incomeVAT <- case V.toList relevantVATAccounts of
              [] -> pure Nothing
              [vatAmount] -> pure $ Just vatAmount
              _ -> undefined
            -- TODO use the conversion rate from the appropriate date
            -- TODO figure out if this is the correct rounding
            incomeCHFAmount <- amountToAmountWithCurrency chfCurrency <$> mapValidationTFailure InputErrorConvert (liftValidation (convertAmount priceGraph RoundNearest currency amount chfCurrency))
            pure $ Just Income {..}
          -- TODO Error
          _ -> undefined
      else pure Nothing

  pure Input {..}

amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
amountToAmountWithCurrency currency amount =
  let Located _ qf = currencyQuantisationFactor currency
      symbol = currencySymbol currency
   in AmountWithCurrency (Amount.format qf amount) symbol

data InputError ann
  = InputErrorConvert (ConvertError ann)

instance ToReport (InputError SourceSpan) where
  toReport = \case
    InputErrorConvert ce -> toReport ce

-- TODO split up "Input" into "Input to Typst" and "VAT report"
data Input = Input
  { inputName :: Text,
    inputQuarter :: !Quarter,
    inputIncome :: Vector Income
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Input)

instance HasCodec Input where
  codec =
    object "Input" $
      Input
        <$> requiredField "name" "name"
          .= inputName
        <*> requiredFieldWith "quarter" (codecViaAeson "Quarter") "quarter"
          .= inputQuarter
        <*> requiredField "income" "income"
          .= inputIncome

data Income = Income
  { incomeDay :: !Day,
    incomeDescription :: !Description,
    incomeAmount :: !AmountWithCurrency,
    incomeVAT :: !(Maybe AmountWithCurrency),
    incomeCHFAmount :: !AmountWithCurrency,
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
        <*> requiredField "vat" "vat"
          .= incomeVAT
        <*> requiredField "chf" "amount in chf"
          .= incomeCHFAmount
        <*> requiredField "evidence" "evidence"
          .= incomeEvidence

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
