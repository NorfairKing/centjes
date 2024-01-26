{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Command.VAT
  ( runCentjesSwitzerlandVAT,
    vatReportInput,
  )
where

import Autodocodec
import Centjes.Command.Check
import Centjes.Compile
import qualified Centjes.Description as Description
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Switzerland.Assets
import Centjes.Switzerland.Constants (development)
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.VAT
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.Typst
import Centjes.Switzerland.Zip
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Conduit
import Control.Monad
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time
import Data.Time.Calendar.Quarter
import Language.Haskell.TH.Load
import Money.Account as Money (Account (..))
import qualified Money.Account as Account
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import Path
import Path.IO
import System.Exit
import System.Process.Typed
import Text.XML as XML

runCentjesSwitzerlandVAT :: Settings -> VATSettings -> IO ()
runCentjesSwitzerlandVAT Settings {..} VATSettings {..} = do
  assetMap <- loadIO assetFileMap
  mainTypContents <- case M.lookup [relfile|vat.typ|] assetMap of
    Nothing -> die "vat.typ template not found."
    Just t -> pure t
  xmlSchemaContents <- case M.lookup [relfile|mwst-schema.xsd|] assetMap of
    Nothing -> die "mwst-schema.xsd asset not found."
    Just t -> pure t
  withSystemTempDir "centjes-switzerland" $ \tdir -> do
    runStderrLoggingT $ do
      -- Produce the input.json structure
      (declarations, diag) <- loadModules $ settingBaseDir </> settingLedgerFile
      ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations
      -- Check ahead of time, so we don't generate reports of invalid ledgers
      val <- liftIO $ runValidationT $ doCompleteCheck declarations
      void $ liftIO $ checkValidation diag val

      validation <- liftIO $ runValidationT $ runReporter $ produceVATReport ledger
      (vatReport, files) <- liftIO $ checkValidation diag validation
      let input = vatReportInput vatReport

      -- Write the xml to a file
      let xmlDoc = xmlReport vatReport
      xmlFile <- do
        xf <- resolveFile tdir "vat.xml"
        liftIO $ XML.writeFile XML.def (fromAbsFile xf) xmlDoc
        when development $ do
          schemaFile <- resolveFile tdir "mwst-schema.xsd"
          liftIO $
            SB.writeFile
              (fromAbsFile schemaFile)
              (TE.encodeUtf8 xmlSchemaContents)
          logInfoN $
            T.pack $
              unwords
                [ "Validating XML output at",
                  fromAbsFile xf,
                  "against schema",
                  fromAbsFile schemaFile
                ]
          runProcess_ $
            setWorkingDir (fromAbsDir tdir) $
              setStdout inherit $
                setStderr inherit $
                  proc
                    "xmllint"
                    [ "--schema",
                      fromAbsFile schemaFile,
                      fromAbsFile xf,
                      "--noout"
                    ]
        pure xf
      logInfoN $
        T.pack $
          unwords
            [ "Wrote XML version to",
              fromAbsFile xmlFile
            ]
      logDebugN $ LT.toStrict $ XML.renderText def xmlDoc

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
        SB.writeFile (fromAbsFile mtf) (TE.encodeUtf8 mainTypContents)
        pure mtf

      liftIO $ compileTypst mainTypFile vatSettingReadmeFile

      logInfoN $
        T.pack $
          unwords
            [ "Typst compilation succesfully created",
              fromAbsFile vatSettingReadmeFile
            ]

      -- Create a nice zip file
      createZipFile vatSettingZipFile $
        M.insert [relfile|README.pdf|] vatSettingReadmeFile $
          M.insert [relfile|raw-input.json|] jsonInputFile $
            M.insert [relfile|vat.xml|] xmlFile $
              M.map (settingBaseDir </>) files

      logInfoN $
        T.pack $
          unwords
            [ "Succesfully created packet",
              fromAbsFile vatSettingZipFile
            ]

vatReportInput :: VATReport ann -> Input
vatReportInput VATReport {..} =
  let inputName = vatReportName
      inputQuarter = vatReportQuarter
      inputRevenues =
        sortOn inputRevenueDay $
          concat
            [ map vatInputDomesticRevenue vatReportDomesticRevenues,
              map vatInputForeignRevenue vatReportForeignRevenues
            ]
      inputExpenses = map vatInputDeductibleExpense vatReportDeductibleExpenses
      inputTotalRevenue = formatAmount vatReportCHF vatReportTotalRevenue
      inputTotalForeignRevenue = formatAmount vatReportCHF vatReportTotalForeignRevenue
      inputDomesticRevenue2023 = formatAmount vatReportCHF vatReportDomesticRevenue2023
      input2023StandardRateVATRevenue = formatAmount vatReportCHF vatReport2023StandardRateVATRevenue
      inputDomesticRevenue2024 = formatAmount vatReportCHF vatReportDomesticRevenue2024
      input2024StandardRateVATRevenue = formatAmount vatReportCHF vatReport2024StandardRateVATRevenue
      inputTotalDomesticRevenue = formatAmount vatReportCHF vatReportTotalDomesticRevenue
      inputTotalVATRevenue = formatAmount vatReportCHF vatReportTotalVATRevenue
      inputPaidVAT = formatAmount vatReportCHF vatReportPaidVAT
      inputPayable = formatAccount vatReportCHF vatReportPayable
   in Input {..}

data Input = Input
  { inputName :: Text,
    inputQuarter :: !Quarter,
    inputRevenues :: ![InputRevenue],
    inputExpenses :: ![InputExpense],
    -- | 200
    --
    -- Total der vereinbarten bzw. vereinnahmten Entgelte, inkl.
    -- optierte Leistungen, Entgelte aus Übertragungen im
    -- Meldeverfahren sowie aus Leistungen im Ausland
    -- (weltweiter Umsatz)
    inputTotalRevenue :: !FormattedAmount,
    -- | 221
    --
    -- Leistungen im Ausland (Ort der Leistung im Ausland)
    inputTotalForeignRevenue :: !FormattedAmount,
    -- | 299
    --
    -- Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)
    inputTotalDomesticRevenue :: !FormattedAmount,
    -- | 302
    --
    -- Leistungen zum Normalsatz 8.1%
    inputDomesticRevenue2023 :: !FormattedAmount,
    input2023StandardRateVATRevenue :: !FormattedAmount,
    -- | 303
    --
    -- Leistungen zum Normalsatz 8.1%
    inputDomesticRevenue2024 :: !FormattedAmount,
    input2024StandardRateVATRevenue :: !FormattedAmount,
    -- | 399
    --
    -- Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)
    inputTotalVATRevenue :: !FormattedAmount,
    -- | 405
    --
    -- Vorsteuer auf Investitionen und übrigem Betriebsaufwand
    inputPaidVAT :: !FormattedAmount,
    -- | 500
    --
    -- Zu bezahlender Betrag
    inputPayable :: !FormattedAccount
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
        <*> requiredField "revenues" "revenues"
          .= inputRevenues
        <*> requiredField "expenses" "expenses"
          .= inputExpenses
        <*> requiredField "total_revenue" "total_revenue"
          .= inputTotalRevenue
        <*> requiredField "total_foreign_revenue" "total foreign revenue"
          .= inputTotalForeignRevenue
        <*> requiredField "total_domestic_revenue" "total domestic revenue"
          .= inputTotalDomesticRevenue
        <*> requiredField "domestic_revenue_2023" "domestic revenue from 2023"
          .= inputDomesticRevenue2023
        <*> requiredField "vat_revenue_standard_2023" "vat_standard"
          .= input2023StandardRateVATRevenue
        <*> requiredField "domestic_revenue_2024" "domestic revenue from 2024"
          .= inputDomesticRevenue2024
        <*> requiredField "vat_revenue_standard_2024" "vat_standard"
          .= input2024StandardRateVATRevenue
        <*> requiredField "total_vat_revenue" "total vat"
          .= inputTotalVATRevenue
        <*> requiredField "vat_paid" "total vat"
          .= inputPaidVAT
        <*> requiredField "payable" "payable"
          .= inputPayable

vatInputDomesticRevenue :: DomesticRevenue ann -> InputRevenue
vatInputDomesticRevenue DomesticRevenue {..} =
  let inputRevenueDay = Timestamp.toDay domesticRevenueTimestamp
      inputRevenueDescription = Description.toText domesticRevenueDescription
      inputRevenueAmount = amountToAmountWithCurrency domesticRevenueCurrency domesticRevenueAmount
      inputRevenueCHFAmount = formatAmount domesticRevenueCurrency domesticRevenueCHFAmount
      inputRevenueVATAmount = Just $ amountToAmountWithCurrency domesticRevenueVATCurrency domesticRevenueVATAmount
      inputRevenueVATCHFAmount = Just $ formatAmount domesticRevenueVATCurrency domesticRevenueVATCHFAmount
      inputRevenueVATRate = Just $ formatVATRate domesticRevenueVATRate
      inputRevenueEvidence = domesticRevenueEvidence
   in InputRevenue {..}

vatInputForeignRevenue :: ForeignRevenue ann -> InputRevenue
vatInputForeignRevenue ForeignRevenue {..} =
  let inputRevenueDay = Timestamp.toDay foreignRevenueTimestamp
      inputRevenueDescription = Description.toText foreignRevenueDescription
      inputRevenueAmount = amountToAmountWithCurrency foreignRevenueCurrency foreignRevenueAmount
      inputRevenueCHFAmount = formatAmount foreignRevenueCurrency foreignRevenueCHFAmount
      inputRevenueVATAmount = Nothing
      inputRevenueVATCHFAmount = Nothing
      inputRevenueVATRate = Nothing
      inputRevenueEvidence = foreignRevenueEvidence
   in InputRevenue {..}

vatInputDeductibleExpense :: DeductibleExpense ann -> InputExpense
vatInputDeductibleExpense DeductibleExpense {..} =
  let inputExpenseDay = Timestamp.toDay deductibleExpenseTimestamp
      inputExpenseDescription = Description.toText deductibleExpenseDescription
      inputExpenseAmount = amountToAmountWithCurrency deductibleExpenseCurrency deductibleExpenseAmount
      inputExpenseCHFAmount = formatAmount deductibleExpenseCurrency deductibleExpenseCHFAmount
      inputExpenseVATAmount = amountToAmountWithCurrency deductibleExpenseVATCurrency deductibleExpenseVATAmount
      inputExpenseVATCHFAmount = formatAmount deductibleExpenseVATCurrency deductibleExpenseVATCHFAmount
      inputExpenseVATRate = formatVATRate deductibleExpenseVATRate
      inputExpenseEvidence = deductibleExpenseEvidence
   in InputExpense {..}

formatVATRate :: VATRate -> String
formatVATRate = \case
  VATRate2023Standard -> "7.7 %"
  VATRate2024Standard -> "8.1 %"
  VATRate2023Reduced -> "2.5 %"
  VATRate2024Reduced -> "2.6 %"
  VATRate2023Hotel -> "3.7 %"
  VATRate2024Hotel -> "3.8 %"

data InputRevenue = InputRevenue
  { inputRevenueDay :: !Day,
    inputRevenueDescription :: !Text,
    inputRevenueAmount :: !AmountWithCurrency,
    inputRevenueCHFAmount :: !FormattedAmount,
    inputRevenueVATAmount :: !(Maybe AmountWithCurrency),
    inputRevenueVATCHFAmount :: !(Maybe FormattedAmount),
    inputRevenueVATRate :: !(Maybe String),
    inputRevenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec InputRevenue)

instance HasCodec InputRevenue where
  codec =
    object "InputRevenue" $
      InputRevenue
        <$> requiredField "day" "day of revenue"
          .= inputRevenueDay
        <*> requiredField "description" "description of revenue"
          .= inputRevenueDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputRevenueAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputRevenueCHFAmount
        <*> optionalField "vat_amount" "VAT amount in original currency"
          .= inputRevenueVATAmount
        <*> optionalField "vat_amount_chf" "VAT amount in chf"
          .= inputRevenueVATCHFAmount
        <*> optionalField "vat_rate" "VAT rate"
          .= inputRevenueVATRate
        <*> requiredField "evidence" "evidence"
          .= inputRevenueEvidence

data InputExpense = InputExpense
  { inputExpenseDay :: !Day,
    inputExpenseDescription :: !Text,
    inputExpenseAmount :: !AmountWithCurrency,
    inputExpenseCHFAmount :: !FormattedAmount,
    inputExpenseVATAmount :: !AmountWithCurrency,
    inputExpenseVATCHFAmount :: !FormattedAmount,
    inputExpenseVATRate :: !String,
    inputExpenseEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec InputExpense)

instance HasCodec InputExpense where
  codec =
    object "InputExpense" $
      InputExpense
        <$> requiredField "day" "day of expense"
          .= inputExpenseDay
        <*> requiredField "description" "description of expense"
          .= inputExpenseDescription
        <*> requiredField "amount" "amount in original currency"
          .= inputExpenseAmount
        <*> requiredField "amount_chf" "amount in chf"
          .= inputExpenseCHFAmount
        <*> requiredField "vat_amount" "VAT amount in original currency"
          .= inputExpenseVATAmount
        <*> requiredField "vat_amount_chf" "VAT amount in chf"
          .= inputExpenseVATCHFAmount
        <*> requiredField "vat_rate" "VAT rate"
          .= inputExpenseVATRate
        <*> requiredField "evidence" "evidence"
          .= inputExpenseEvidence

data AmountWithCurrency = AmountWithCurrency
  { amountWithCurrencyAmount :: FormattedAmount,
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

amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
amountToAmountWithCurrency currency amount =
  let Located _ qf = currencyQuantisationFactor currency
      symbol = currencySymbol currency
   in AmountWithCurrency (Amount.format qf amount) symbol

type FormattedAmount = String

formatAmount :: Currency ann -> Money.Amount -> FormattedAmount
formatAmount currency account =
  let Located _ qf = currencyQuantisationFactor currency
   in Amount.format qf account

type FormattedAccount = String

formatAccount :: Currency ann -> Money.Account -> FormattedAccount
formatAccount currency account =
  let Located _ qf = currencyQuantisationFactor currency
   in Account.format qf account

xmlReport :: VATReport ann -> XML.Document
xmlReport VATReport {} =
  XML.Document
    { documentPrologue =
        XML.Prologue
          { prologueBefore = [],
            prologueDoctype = Nothing,
            prologueAfter = []
          },
      documentRoot =
        XML.Element
          { elementName = "test",
            elementAttributes = M.empty,
            elementNodes = []
          },
      documentEpilogue = []
    }
