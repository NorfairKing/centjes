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
import Centjes.Compile
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.Switzerland.OptParse
import Centjes.Switzerland.Report.VAT
import Centjes.Switzerland.Reporter
import Centjes.Switzerland.Templates
import Centjes.Switzerland.Typst
import Centjes.Switzerland.Zip
import Centjes.Validation
import Conduit
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar.Quarter
import Language.Haskell.TH.Load
import Money.Account as Money (Account (..))
import qualified Money.Account as Account
import Money.Amount as Money (Amount (..))
import qualified Money.Amount as Amount
import Path
import Path.IO
import System.Exit

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
      ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations

      validation <- liftIO $ runValidationT $ runReporter $ produceVATReport ledger
      (vatReport, files) <- liftIO $ checkValidation diag validation
      let input = vatReportInput vatReport

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
      inputTotalRevenue = formatAmount vatReportCHF vatReportTotalRevenue
      inputForeignRevenue = formatAmount vatReportCHF vatReportForeignRevenue
      inputDomesticRevenue = formatAmount vatReportCHF vatReportDomesticRevenue
      inputStandardRateVAT81PercentRevenue = formatAmount vatReportCHF vatReportStandardRateVAT81PercentRevenue
      inputTotalVATRevenue = formatAmount vatReportCHF vatReportTotalVATRevenue
      inputPaidVAT = formatAmount vatReportCHF vatReportPaidVAT
      inputPayable = formatAccount vatReportCHF vatReportPayable
   in Input {..}

data Input = Input
  -- TODO make these amounts without currency
  { inputName :: Text,
    inputQuarter :: !Quarter,
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
    inputForeignRevenue :: !FormattedAmount,
    -- | 299
    --
    -- Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)
    inputDomesticRevenue :: !FormattedAmount,
    -- | 302
    --
    -- Leistungen zum Normalsatz 8.1%
    inputStandardRateVAT81PercentRevenue :: !FormattedAmount,
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
        <*> requiredField "total_revenue" "total_revenue"
          .= inputTotalRevenue
        <*> requiredField "foreign_revenue" "foreign_revenue"
          .= inputForeignRevenue
        <*> requiredField "domestic_revenue" "domestic_revenue"
          .= inputDomesticRevenue
        <*> requiredField "vat_revenue_standard" "vat_standard"
          .= inputStandardRateVAT81PercentRevenue
        <*> requiredField "total_vat_revenue" "total vat"
          .= inputTotalVATRevenue
        <*> requiredField "vat_paid" "total vat"
          .= inputPaidVAT
        <*> requiredField "payable" "payable"
          .= inputPayable

-- data AmountWithCurrency = AmountWithCurrency
--   { amountWithCurrencyAmount :: FormattedAmount,
--     amountWithCurrencyCurrency :: CurrencySymbol
--   }
--   deriving (Show, Eq)
--   deriving (FromJSON, ToJSON) via (Autodocodec AmountWithCurrency)
--
-- instance HasCodec AmountWithCurrency where
--   codec =
--     object "AmountWithCurrency" $
--       AmountWithCurrency
--         <$> requiredField "formatted" "formatted amount"
--         .= amountWithCurrencyAmount
--         <*> requiredField "symbol" "currency symbol"
--         .= amountWithCurrencyCurrency
--
-- amountToAmountWithCurrency :: Currency ann -> Money.Amount -> AmountWithCurrency
-- amountToAmountWithCurrency currency amount =
--   let Located _ qf = currencyQuantisationFactor currency
--       symbol = currencySymbol currency
--    in AmountWithCurrency (Amount.format qf amount) symbol

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
