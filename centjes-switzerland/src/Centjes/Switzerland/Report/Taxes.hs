{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes
  ( TaxesInput (..),
    TaxesReport (..),
    Revenue (..),
    TaxesError (..),
    produceTaxesReport,

    -- * XML report
    XMLReport (..),
    xmlReportDocument,
    xmlRenderSettings,
    produceXMLReport,

    -- * Typst report
    Input (..),
    taxesReportInput,
  )
where

import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.Taxes.ETax
import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.Report.Taxes.Typst
import Centjes.Switzerland.Report.VATRate
import Centjes.Switzerland.Reporter
import Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Money.Account as Account
import qualified Money.Account as Money (Account (..))
import qualified Money.Amount as Amount
import qualified Money.Amount as Money (Amount, Rounding (..))
import qualified Money.QuantisationFactor as QuantisationFactor
import Numeric.Natural
import Path

produceTaxesReport ::
  Ord ann =>
  TaxesInput ->
  Ledger ann ->
  Reporter (TaxesError ann) (TaxesReport ann)
produceTaxesReport TaxesInput {..} Ledger {..} = do
  let taxesReportLastName = taxesInputLastName
  let taxesReportFirstName = taxesInputFirstName
  let taxesReportYear = taxesInputYear
  let taxesReportInsuredPersonNumber = taxesInputInsuredPersonNumber

  let chfSymbol = CurrencySymbol "CHF"
  taxesReportCHF <- case M.lookup chfSymbol ledgerCurrencies of
    Nothing -> validationTFailure TaxesErrorNoCHF
    Just lqf@(Located _ qf) ->
      if Just qf == QuantisationFactor.fromWord32 100
        then pure $ Currency chfSymbol lqf
        else validationTFailure $ TaxesErrorWrongCHF lqf

  -- TODO: Let the user pass in the correct rates, don't just use the ones they used.
  -- We can't build the rates into the binary because there are MANY Currencies
  -- that could be relevant.
  -- So we let the user download rates with download-rates instead.
  let dailyPriceGraphs = pricesToDailyPriceGraphs ledgerPrices

  taxesReportRevenues <- fmap concat $
    forM (V.toList ledgerTransactions) $
      \(Located tl Transaction {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        if dayInYear taxesReportYear day
          then do
            -- Every posting and the next
            let postingsTups =
                  let l = V.toList transactionPostings
                   in zip l (map Just (tail l) ++ [Nothing])

            fmap catMaybes $
              forM postingsTups $
                \(Located pl1 p1, mP2) -> do
                  let Located _ accountName = postingAccountName p1
                  if accountName `elem` taxesInputIncomeAccounts
                    then fmap Just $ do
                      let revenueTimestamp = timestamp
                      revenueDescription <- requireDescription transactionDescription
                      revenueEvidence <- requireEvidence tl [reldir|income|] transactionAttachments
                      let Located _ revenueCurrency = postingCurrency p1
                      let Located al1 account = postingAccount p1
                      revenueAmount <- requireNegative tl pl1 account
                      revenueCHFAmount <- convertDaily al1 dailyPriceGraphs day revenueCurrency taxesReportCHF revenueAmount

                      case mP2 of
                        Nothing -> validationTFailure TaxesErrorNoVATPosting
                        Just (Located pl2 p2) -> do
                          let Located al2 vatAccountName = postingAccountName p2
                          when (vatAccountName /= taxesInputVATIncomeAccountName) $ validationTFailure TaxesErrorVATPostingNotVATAccount
                          let Located _ revenueVATCurrency = postingCurrency p2
                          let Located _ vatAccount = postingAccount p2
                          -- TODO require that the vat currency is the same?
                          revenueVATAmount <- requireNegative tl pl2 vatAccount
                          revenueVATCHFAmount <- convertDaily al2 dailyPriceGraphs day revenueVATCurrency taxesReportCHF revenueVATAmount

                          revenueVATRate <- requirePercentageVATRate tl (postingPercentage p2)
                          pure Revenue {..}
                    else pure Nothing
          else pure []

  taxesReportSelfEmploymentRevenue <- requireSumAmount (map revenueNettoCHFAmount taxesReportRevenues)

  pure TaxesReport {..}

dayInYear :: Year -> Day -> Bool
dayInYear y d =
  let (y', _, _) = toGregorian d
   in y == y'

requireDescription ::
  Maybe (GenLocated ann Description) ->
  Reporter (TaxesError ann) Description
requireDescription = \case
  Nothing -> validationTFailure TaxesErrorNoDescription
  Just (Located _ d) -> pure d

requireSumAmount ::
  Foldable f =>
  f Money.Amount ->
  Reporter (TaxesError ann) Money.Amount
requireSumAmount amounts =
  case Amount.sum amounts of
    Nothing -> validationTFailure $ TaxesErrorSum $ Foldable.toList amounts
    Just a -> pure a

requireNegative ::
  ann ->
  ann ->
  Money.Account ->
  Reporter (TaxesError ann) Money.Amount
requireNegative tl pl account =
  case account of
    Money.Positive _ -> validationTFailure $ TaxesErrorPositiveIncome tl pl account
    Money.Negative a -> pure a

convertDaily ::
  Ord ann =>
  ann ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Day ->
  Currency ann ->
  Currency ann ->
  Money.Amount ->
  Reporter (TaxesError ann) Money.Amount
convertDaily al dailyPrices day currencyFrom currencyTo amount =
  if currencyFrom == currencyTo
    then pure amount
    else case M.lookupLE day dailyPrices of
      Nothing -> validationTFailure $ TaxesErrorCouldNotConvert al currencyFrom currencyTo amount
      Just (_, mpg) ->
        case MemoisedPriceGraph.lookup mpg currencyFrom currencyTo of
          Nothing -> validationTFailure $ TaxesErrorCouldNotConvert al currencyFrom currencyTo amount
          Just rate -> do
            let Located _ qfFrom = currencyQuantisationFactor currencyFrom
            let Located _ qfTo = currencyQuantisationFactor currencyTo
            let (mA, _) = Amount.convert Money.RoundNearest qfFrom amount rate qfTo
            case mA of
              Nothing -> validationTFailure $ TaxesErrorCouldNotConvert al currencyFrom currencyTo amount
              Just convertedAmount -> pure convertedAmount

requireEvidence ::
  ann ->
  Path Rel Dir ->
  Vector (GenLocated ann (Attachment ann)) ->
  Reporter (TaxesError ann) (NonEmpty (Path Rel File))
requireEvidence tl subdir attachments =
  case NE.nonEmpty (V.toList attachments) of
    Nothing -> validationTFailure $ TaxesErrorNoEvidence tl
    Just ne ->
      forM ne $ \(Located _ (Attachment (Located _ rf))) -> do
        let pathInTarball = subdir </> rf
        includeFile pathInTarball rf
        pure pathInTarball

requirePercentageRate ::
  ann ->
  Maybe (GenLocated ann (Percentage ann)) ->
  Reporter (TaxesError ann) (ann, Ratio Natural)
requirePercentageRate tl = \case
  Nothing -> validationTFailure $ TaxesErrorNoVATPercentage tl
  Just (Located pl (Percentage (Located _ r))) -> pure (pl, r)

requirePercentageVATRate ::
  ann ->
  Maybe (GenLocated ann (Percentage ann)) ->
  Reporter (TaxesError ann) VATRate
requirePercentageVATRate tl mp = do
  (pl, r) <- requirePercentageRate tl mp
  requireRatioVATRate tl pl r

requireRatioVATRate ::
  ann ->
  ann ->
  Ratio Natural ->
  Reporter (TaxesError ann) VATRate
requireRatioVATRate tl pl r = case r of
  0.077 -> pure VATRate2023Standard
  0.081 -> pure VATRate2024Standard
  0.025 -> pure VATRate2023Reduced
  0.026 -> pure VATRate2024Reduced
  0.037 -> pure VATRate2023Hotel
  0.038 -> pure VATRate2024Hotel
  _ -> validationTFailure $ TaxesErrorUnknownVATRate tl pl r

-- TODO move this into a shared module
vatRateRatio :: VATRate -> Ratio Natural
vatRateRatio = \case
  VATRate2023Standard -> 0.077
  VATRate2024Standard -> 0.081
  VATRate2023Reduced -> 0.025
  VATRate2024Reduced -> 0.026
  VATRate2023Hotel -> 0.037
  VATRate2024Hotel -> 0.038
