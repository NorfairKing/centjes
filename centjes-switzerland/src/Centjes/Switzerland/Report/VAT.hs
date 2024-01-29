{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VAT
  ( VATReport (..),
    DomesticRevenue (..),
    ForeignRevenue (..),
    DeductibleExpense (..),
    VATRate (..),
    VATError (..),
    produceVATReport,
  )
where

import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.VAT.Types
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.Writer
import Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Time.Calendar.Quarter
import Data.Validity.Time ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import qualified Money.Account as Account
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Path

produceVATReport ::
  Ord ann =>
  Ledger ann ->
  Reporter (VATError ann) (VATReport ann)
produceVATReport Ledger {..} = do
  -- TODO make these configurable
  let vatReportName = "Tom Sydney Kerckhove"
  let vatReportQuarter = YearQuarter 2024 Q1
  let domesticIncomeAccountName = "income:domestic"
  let foreignIncomeAccountName = "income:foreign"
  let vatIncomeAccountName = "income:vat"
  let vatExpensesAccountName = "expenses:vat"

  let chfSymbol = CurrencySymbol "CHF"
  vatReportCHF <- case M.lookup chfSymbol ledgerCurrencies of
    Nothing -> validationTFailure VATErrorNoCHF
    Just lqf -> pure $ Currency chfSymbol lqf

  -- TODO: Let the user pass in the correct rates, don't just use the ones they used.
  -- We can't build the rates into the binary because there are MANY Currencies
  -- that could be relevant.
  -- So we let the user download rates with download-rates instead.
  let dailyPriceGraphs = pricesToDailyPriceGraphs ledgerPrices

  vatReportDomesticRevenues <- fmap concat $
    forM (V.toList ledgerTransactions) $
      \(Located tl Transaction {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        if dayInQuarter vatReportQuarter day
          then do
            -- Every posting and the next
            let postingsTups =
                  let l = V.toList transactionPostings
                   in zip l (map Just (tail l) ++ [Nothing])

            fmap catMaybes $
              forM postingsTups $
                \(Located pl1 p1, mP2) -> do
                  let Located _ accountName = postingAccountName p1
                  if accountName == domesticIncomeAccountName
                    then fmap Just $ do
                      let domesticRevenueTimestamp = timestamp
                      domesticRevenueDescription <- requireDescription transactionDescription
                      domesticRevenueEvidence <- requireEvidence tl [reldir|income|] transactionAttachments
                      let Located _ domesticRevenueCurrency = postingCurrency p1
                      let Located al1 account = postingAccount p1
                      domesticRevenueAmount <- requireNegative tl pl1 account
                      domesticRevenueCHFAmount <- convertDaily al1 dailyPriceGraphs day domesticRevenueCurrency vatReportCHF domesticRevenueAmount

                      case mP2 of
                        Nothing -> validationTFailure VATErrorNoVATPosting
                        Just (Located pl2 p2) -> do
                          let Located al2 vatAccountName = postingAccountName p2
                          when (vatAccountName /= vatIncomeAccountName) $ validationTFailure VATErrorVATPostingNotVATAccount
                          let Located _ domesticRevenueVATCurrency = postingCurrency p2
                          let Located _ vatAccount = postingAccount p2
                          -- TODO require that the vat currency is the same?
                          domesticRevenueVATAmount <- requireNegative tl pl2 vatAccount
                          domesticRevenueVATCHFAmount <- convertDaily al2 dailyPriceGraphs day domesticRevenueVATCurrency vatReportCHF domesticRevenueVATAmount
                          domesticRevenueVATRate <- requireVATRate tl (postingPercentage p2)
                          pure DomesticRevenue {..}
                    else pure Nothing
          else pure []

  vatReportTotalDomesticRevenue <-
    requireSumAmount
      (map domesticRevenueCHFAmount vatReportDomesticRevenues)

  vatReport2023StandardRateVATRevenue <-
    requireSumAmount
      ( map
          domesticRevenueVATCHFAmount
          (filter ((== VATRate2023Standard) . domesticRevenueVATRate) vatReportDomesticRevenues)
      )
  vatReportDomesticRevenue2023 <-
    requireSumAmount
      ( map
          domesticRevenueCHFAmount
          (filter ((== VATRate2023Standard) . domesticRevenueVATRate) vatReportDomesticRevenues)
      )
  vatReport2024StandardRateVATRevenue <-
    requireSumAmount
      ( map
          domesticRevenueVATCHFAmount
          (filter ((== VATRate2024Standard) . domesticRevenueVATRate) vatReportDomesticRevenues)
      )
  vatReportDomesticRevenue2024 <-
    requireSumAmount
      ( map
          domesticRevenueCHFAmount
          (filter ((== VATRate2024Standard) . domesticRevenueVATRate) vatReportDomesticRevenues)
      )

  vatReportForeignRevenues <- fmap concat $
    forM (V.toList ledgerTransactions) $
      \(Located tl Transaction {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        if dayInQuarter vatReportQuarter day
          then do
            -- TODO assert that the rate is one of the common ones

            fmap catMaybes $
              forM (V.toList transactionPostings) $
                \(Located pl Posting {..}) -> do
                  let Located _ accountName = postingAccountName
                  if accountName == foreignIncomeAccountName
                    then fmap Just $ do
                      let foreignRevenueTimestamp = timestamp
                      foreignRevenueEvidence <- requireEvidence tl [reldir|income|] transactionAttachments
                      foreignRevenueDescription <- requireDescription transactionDescription
                      let Located _ foreignRevenueCurrency = postingCurrency
                      let Located al account = postingAccount
                      foreignRevenueAmount <- requireNegative tl pl account
                      foreignRevenueCHFAmount <- convertDaily al dailyPriceGraphs day foreignRevenueCurrency vatReportCHF foreignRevenueAmount
                      pure $ ForeignRevenue {..}
                    else pure Nothing
          else pure []

  vatReportTotalForeignRevenue <- requireSumAmount $ map foreignRevenueCHFAmount vatReportForeignRevenues

  vatReportTotalRevenue <- requireAddAmount vatReportTotalDomesticRevenue vatReportTotalForeignRevenue

  vatReportTotalVATRevenue <-
    requireSumAmount
      [ vatReport2023StandardRateVATRevenue,
        vatReport2024StandardRateVATRevenue
      ]

  vatReportDeductibleExpenses <- fmap concat $
    forM (V.toList ledgerTransactions) $
      \(Located tl Transaction {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        if dayInQuarter vatReportQuarter day
          then fmap catMaybes $ do
            -- Every posting and the next
            let postingsTups =
                  let l = V.toList transactionPostings
                   in zip l (map Just (tail l) ++ [Nothing])
            forM postingsTups $ \(Located pl1 p1, mP2) -> do
              -- TODO check if this account name checks out as well ?
              -- let Located _ accountName = postingAccountName p1
              -- Maybe just assume that any declared vat is deductible
              case mP2 of
                Nothing -> pure Nothing
                Just (Located pl2 p2) -> do
                  let Located _ vatAccountName = postingAccountName p2
                  if vatAccountName == vatExpensesAccountName
                    then fmap Just $ do
                      let Located _ deductibleExpenseCurrency = postingCurrency p1
                      let Located al1 account = postingAccount p1
                      deductibleExpenseAmount <- requirePositive tl pl1 account
                      deductibleExpenseCHFAmount <- convertDaily al1 dailyPriceGraphs day deductibleExpenseCurrency vatReportCHF deductibleExpenseAmount
                      let deductibleExpenseTimestamp = timestamp
                      deductibleExpenseDescription <- requireDescription transactionDescription
                      deductibleExpenseEvidence <- requireEvidence tl [reldir|deductions|] transactionAttachments
                      let Located _ deductibleExpenseVATCurrency = postingCurrency p2
                      let Located al vatAccount = postingAccount p2
                      deductibleExpenseVATAmount <- requirePositive tl pl2 vatAccount
                      deductibleExpenseVATCHFAmount <- convertDaily al dailyPriceGraphs day deductibleExpenseVATCurrency vatReportCHF deductibleExpenseVATAmount
                      deductibleExpenseVATRate <- requireVATRate tl (postingPercentage p2)
                      pure DeductibleExpense {..}
                    else pure Nothing
          else pure []

  vatReportPaidVAT <- requireSumAmount (map deductibleExpenseVATCHFAmount vatReportDeductibleExpenses)

  vatReportPayable <- case Account.subtract (Account.fromAmount vatReportTotalVATRevenue) (Account.fromAmount vatReportPaidVAT) of
    Nothing -> validationTFailure $ VATErrorSubtract vatReportTotalRevenue vatReportPaidVAT
    Just a -> pure a

  pure VATReport {..}

dayInQuarter :: Quarter -> Day -> Bool
dayInQuarter quarter day =
  day >= periodFirstDay quarter && day <= periodLastDay quarter

requireDescription ::
  Maybe (GenLocated ann Description) ->
  Reporter (VATError ann) Description
requireDescription = \case
  Nothing -> validationTFailure VATErrorNoDescription
  Just (Located _ d) -> pure d

requireEvidence ::
  ann ->
  Path Rel Dir ->
  Vector (GenLocated ann (Attachment ann)) ->
  Reporter (VATError ann) (NonEmpty (Path Rel File))
requireEvidence tl subdir attachments =
  case NE.nonEmpty (V.toList attachments) of
    Nothing -> validationTFailure $ VATErrorNoEvidence tl
    Just ne ->
      forM ne $ \(Located _ (Attachment (Located _ rf))) -> do
        let pathInTarball = subdir </> rf
        includeFile pathInTarball rf
        pure pathInTarball

requireNegative ::
  ann ->
  ann ->
  Money.Account ->
  Reporter (VATError ann) Money.Amount
requireNegative tl pl account =
  case account of
    Positive _ -> validationTFailure $ VATErrorPositiveIncome tl pl account
    Negative a -> pure a

requirePositive ::
  ann ->
  ann ->
  Money.Account ->
  Reporter (VATError ann) Money.Amount
requirePositive tl pl account =
  case account of
    Negative _ -> validationTFailure $ VATErrorNegativeExpense tl pl account
    Positive a -> pure a

requireAddAmount ::
  Money.Amount ->
  Money.Amount ->
  Reporter (VATError ann) Money.Amount
requireAddAmount a1 a2 =
  case Amount.add a1 a2 of
    Nothing -> validationTFailure $ VATErrorAdd a1 a2
    Just a -> pure a

requireSumAmount ::
  Foldable f =>
  f Money.Amount ->
  Reporter (VATError ann) Money.Amount
requireSumAmount amounts =
  case Amount.sum amounts of
    Nothing -> validationTFailure $ VATErrorSum $ Foldable.toList amounts
    Just a -> pure a

convertDaily ::
  Ord ann =>
  ann ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Day ->
  Currency ann ->
  Currency ann ->
  Money.Amount ->
  Reporter (VATError ann) Money.Amount
convertDaily al dailyPrices day currencyFrom currencyTo amount =
  if currencyFrom == currencyTo
    then pure amount
    else case M.lookupLE day dailyPrices of
      Nothing -> validationTFailure $ VATErrorCouldNotConvert al currencyFrom currencyTo amount
      Just (_, mpg) ->
        case MemoisedPriceGraph.lookup mpg currencyFrom currencyTo of
          Nothing -> validationTFailure $ VATErrorCouldNotConvert al currencyFrom currencyTo amount
          Just rate -> do
            let Located _ qfFrom = currencyQuantisationFactor currencyFrom
            let Located _ qfTo = currencyQuantisationFactor currencyTo
            let (mA, _) = Amount.convert RoundNearest qfFrom amount rate qfTo
            case mA of
              Nothing -> validationTFailure $ VATErrorCouldNotConvert al currencyFrom currencyTo amount
              Just convertedAmount -> pure convertedAmount

requireVATRate ::
  ann ->
  Maybe (GenLocated ann (Percentage ann)) ->
  Reporter (VATError ann) VATRate
requireVATRate tl = \case
  Nothing -> validationTFailure VATErrorNoVATPercentage
  Just (Located pl (Percentage (Located _ r))) -> case r of
    0.077 -> pure VATRate2023Standard
    0.081 -> pure VATRate2024Standard
    0.025 -> pure VATRate2023Reduced
    0.026 -> pure VATRate2024Reduced
    0.037 -> pure VATRate2023Hotel
    0.038 -> pure VATRate2024Hotel
    _ -> validationTFailure $ VATErrorUnknownVATRate tl pl r
