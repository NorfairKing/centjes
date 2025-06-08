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
  ( VATInput (..),
    VATReport (..),
    DomesticRevenue (..),
    ForeignRevenue (..),
    DeductibleExpense (..),
    VATRate (..),
    VATError (..),
    produceVATReport,

    -- * XML report
    XMLReport (..),
    xmlReportDocument,
    xmlRenderSettings,
    produceXMLReport,

    -- * Typst report
    Input (..),
    vatReportInput,
  )
where

import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.VAT.EMWST
import Centjes.Switzerland.Report.VAT.Types
import Centjes.Switzerland.Report.VAT.Typst
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Applicative
import Control.Monad
import Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Time.Calendar.Quarter
import Data.Validity
import Data.Validity.Time ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Money.Account as Account
import qualified Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import qualified Money.QuantisationFactor as QuantisationFactor
import Path

produceVATReport ::
  (Validity ann, Show ann, Ord ann) =>
  VATInput ->
  Ledger ann ->
  Reporter (VATError ann) (VATReport ann)
produceVATReport vatInput@VATInput {..} ledger@Ledger {..} = do
  let vatReportPersonName = vatInputPersonName
  let vatReportOrganisationName = vatInputOrganisationName
  let vatReportVATId = vatInputVATId
  let vatReportQuarter = vatInputQuarter

  let chfSymbol = CurrencySymbol "CHF"
  vatReportCHF <- case M.lookup chfSymbol ledgerCurrencies of
    Nothing -> validationTFailure VATErrorNoCHF
    Just lqf@(Located _ qf) ->
      if Just qf == QuantisationFactor.fromWord32 100
        then pure $ Currency chfSymbol lqf
        else validationTFailure $ VATErrorWrongCHF lqf

  when (not (M.member vatInputTagDeductible ledgerTags)) $ validationTFailure VATErrorNoTagDeductible
  when (not (M.member vatInputTagNotDeductible ledgerTags)) $ validationTFailure VATErrorNoTagNotDeductible

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
            let postingsTups = consequtiveTups $ V.toList transactionPostings

            fmap catMaybes $
              forM postingsTups $
                \(Located pl1 p1, mP2) -> do
                  let Located _ accountName = postingAccountName p1
                  if accountName == vatInputDomesticIncomeAccountName
                    then fmap Just $ do
                      let domesticRevenueTimestamp = timestamp
                      domesticRevenueDescription <- requireDescription transactionDescription
                      domesticRevenueEvidence <- requireEvidence tl [reldir|income|] transactionAttachments
                      let Located _ domesticRevenueCurrency = postingCurrency p1
                      let Located al1 account = postingAccount p1
                      domesticRevenueAmount <- requireNegative tl pl1 account
                      domesticRevenueCHFAmount <- convertDaily al1 dailyPriceGraphs day domesticRevenueCurrency vatReportCHF domesticRevenueAmount

                      case mP2 of
                        Nothing -> validationTFailure $ VATErrorNoVATPosting tl pl1
                        Just (Located pl2 p2) -> do
                          let Located al2 vatAccountName = postingAccountName p2
                          when (vatAccountName /= vatInputVATIncomeAccountName) $ validationTFailure $ VATErrorVATPostingNotVATAccount tl pl2
                          let Located _ domesticRevenueVATCurrency = postingCurrency p2
                          let Located _ vatAccount = postingAccount p2
                          -- TODO require that the vat currency is the same?
                          domesticRevenueVATAmount <- requireNegative tl pl2 vatAccount
                          domesticRevenueVATCHFAmount <- convertDaily al2 dailyPriceGraphs day domesticRevenueVATCurrency vatReportCHF domesticRevenueVATAmount

                          domesticRevenueVATRate <- requirePercentageVATRate tl (postingAmountRatio p2)
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

  let foreignRevenuesOfAccount foreignAccountName = fmap concat $
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
                      if accountName == foreignAccountName
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

  vatReportForeignRevenues <- foreignRevenuesOfAccount vatInputForeignIncomeAccountName

  vatReportTotalForeignRevenue <- requireSumAmount $ map foreignRevenueCHFAmount vatReportForeignRevenues

  vatReportExportsRevenues <- foreignRevenuesOfAccount vatInputExportsIncomeAccountName
  vatReportTotalExportsRevenue <- requireSumAmount $ map foreignRevenueCHFAmount vatReportExportsRevenues

  vatReportTotalForeignDeductions <- requireAddAmount vatReportTotalForeignRevenue vatReportTotalExportsRevenue

  vatReportTotalRevenue <- requireAddAmount vatReportTotalDomesticRevenue vatReportTotalForeignDeductions

  vatReportTotalVATRevenue <-
    requireSumAmount
      [ vatReport2023StandardRateVATRevenue,
        vatReport2024StandardRateVATRevenue
      ]

  vatReportDeductibleExpenses <-
    gatherDeductibleExpenses
      vatInput
      ledger
      vatReportQuarter
      vatReportCHF
      dailyPriceGraphs

  vatReportPaidVAT <- requireSumAmount (map deductibleExpenseVATCHFAmount vatReportDeductibleExpenses)
  let vatReportTotalVATDeductions = vatReportPaidVAT

  vatReportPayable <- case Account.subtract (Account.fromAmount vatReportTotalVATRevenue) (Account.fromAmount vatReportPaidVAT) of
    Nothing -> validationTFailure $ VATErrorSubtract vatReportTotalRevenue vatReportPaidVAT
    Just a -> pure a

  let unValidatedVatReport = VATReport {..}
  case prettyValidate unValidatedVatReport of
    Left err -> validationTFailure $ VATErrorReportInvalid unValidatedVatReport err
    Right vr -> pure vr

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
        let pathInTarball = subdir </> simplifyDir rf
        includeFile pathInTarball rf
        pure pathInTarball

requireNegative ::
  ann ->
  ann ->
  Money.Account ->
  Reporter (VATError ann) Money.Amount
requireNegative tl pl account =
  case account of
    Money.Positive _ -> validationTFailure $ VATErrorPositiveIncome tl pl account
    Money.Negative a -> pure a

requirePositive ::
  ann ->
  ann ->
  Money.Account ->
  Reporter (VATError ann) Money.Amount
requirePositive tl pl account =
  case account of
    Money.Negative _ -> validationTFailure $ VATErrorNegativeExpense tl pl account
    Money.Positive a -> pure a

requireAddAmount ::
  Money.Amount ->
  Money.Amount ->
  Reporter (VATError ann) Money.Amount
requireAddAmount a1 a2 =
  case Amount.add a1 a2 of
    Nothing -> validationTFailure $ VATErrorAdd a1 a2
    Just a -> pure a

requireSumAmount ::
  (Foldable f) =>
  f Money.Amount ->
  Reporter (VATError ann) Money.Amount
requireSumAmount amounts =
  case Amount.sum amounts of
    Nothing -> validationTFailure $ VATErrorSum $ Foldable.toList amounts
    Just a -> pure a

convertDaily ::
  (Ord ann) =>
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

requirePercentageRate ::
  ann ->
  Maybe (GenLocated ann (AmountRatio ann)) ->
  Reporter (VATError ann) (ann, Rational)
requirePercentageRate tl = \case
  Nothing -> validationTFailure $ VATErrorNoVATPercentage tl
  Just (Located pl (AmountRatio _ _ (Located _ r))) -> pure (pl, r)

requirePercentageVATRate ::
  ann ->
  Maybe (GenLocated ann (AmountRatio ann)) ->
  Reporter (VATError ann) VATRate
requirePercentageVATRate tl mp = do
  (pl, r) <- requirePercentageRate tl mp
  requireRatioVATRate tl pl r

requireRatioVATRate ::
  ann ->
  ann ->
  Rational ->
  Reporter (VATError ann) VATRate
requireRatioVATRate tl pl r = case r of
  0.077 -> pure VATRate2023Standard
  0.081 -> pure VATRate2024Standard
  0.025 -> pure VATRate2023Reduced
  0.026 -> pure VATRate2024Reduced
  0.037 -> pure VATRate2023Hotel
  0.038 -> pure VATRate2024Hotel
  _ -> validationTFailure $ VATErrorUnknownVATRate tl pl r

vatRateRatio :: VATRate -> Rational
vatRateRatio = \case
  VATRate2023Standard -> 0.077
  VATRate2024Standard -> 0.081
  VATRate2023Reduced -> 0.025
  VATRate2024Reduced -> 0.026
  VATRate2023Hotel -> 0.037
  VATRate2024Hotel -> 0.038

gatherDeductibleExpenses ::
  (Ord ann) =>
  VATInput ->
  Ledger ann ->
  Quarter ->
  Currency ann ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Reporter (VATError ann) [DeductibleExpense ann]
gatherDeductibleExpenses vatInput@VATInput {..} Ledger {..} quarter chf dailyPriceGraphs =
  fmap (concatMap (maybe [] NE.toList)) $
    forM (V.toList ledgerTransactions) $
      \lt@(Located tl Transaction {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        if not $ dayInQuarter quarter day
          then pure (Nothing :: Maybe (NonEmpty (DeductibleExpense ann)))
          else do
            let mDeductibleTag = M.lookup vatInputTagDeductible transactionTags
            let mNotDeductibleTag = M.lookup vatInputTagNotDeductible transactionTags
            let mVATDeductibleTag = M.lookup vatInputTagVATDeductible transactionTags
            let mNotVATDeductibleTag = M.lookup vatInputTagNotVATDeductible transactionTags

            case (mVATDeductibleTag <|> mDeductibleTag, mNotVATDeductibleTag <|> mNotDeductibleTag) of
              -- Can't tag as both deductible and not-deductible
              (Just tagl, Just tagnotl) -> validationTFailure $ VATErrorDeductibleAndNotDeductible tl tagl tagnotl
              -- Ignore the transaction if it's tagged as not-deductible
              (Nothing, Just _) -> pure Nothing
              -- If it's tagged as deductible, expect at least one deductible expense
              (Just tagl, Nothing) -> do
                mDes <- parseExpectedDeductibleExpenses vatInput ledgerAccounts dailyPriceGraphs chf lt
                case mDes of
                  Nothing -> validationTFailure $ VATErrorDeductibleNoExpenses tl tagl
                  Just ne -> pure $ Just ne
              -- If it's not tagged at all, error if there are expenses.
              (Nothing, Nothing) ->
                case parseUnxpectedDeductibleExpenses vatInput ledgerAccounts lt of
                  Nothing -> pure Nothing
                  Just lp -> validationTFailure $ VATErrorUntaggedExpenses tl lp

parseUnxpectedDeductibleExpenses ::
  VATInput ->
  Map AccountName (GenLocated ann (Account ann)) ->
  GenLocated ann (Transaction ann) ->
  Maybe (GenLocated ann (Posting ann))
parseUnxpectedDeductibleExpenses VATInput {..} accounts (Located _ Transaction {..}) =
  (listToMaybe . catMaybes) $ flip map (consequtiveTups (V.toList transactionPostings)) $ \(lp1@(Located _ p1), mP2) ->
    let Located _ accountName = postingAccountName p1
        Located _ account = postingAccount p1
     in case M.lookup accountName accounts of
          Nothing -> undefined -- Undeclared account, should not happen.
          Just (Located _ acc) -> case accountType acc of
            AccountTypeExpenses | account >= Account.zero ->
              case mP2 of
                Nothing ->
                  -- If this IS the VAT posting, it's fine.
                  if accountName == vatInputVATExpensesAccountName
                    then Nothing
                    else Just lp1
                Just (Located _ p2) ->
                  let Located _ accountName2 = postingAccountName p2
                   in -- If the second posting IS the VAT posting, it's fine.
                      if accountName2 == vatInputExportsIncomeAccountName
                        then Nothing
                        else Just lp1
            _ -> Nothing

parseExpectedDeductibleExpenses ::
  (Ord ann) =>
  VATInput ->
  Map AccountName (GenLocated ann (Account ann)) ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Currency ann ->
  GenLocated ann (Transaction ann) ->
  Reporter (VATError ann) (Maybe (NonEmpty (DeductibleExpense ann)))
parseExpectedDeductibleExpenses VATInput {..} accounts dailyPriceGraphs chf (Located tl Transaction {..}) =
  fmap (NE.nonEmpty . catMaybes) $ forM (consequtiveTups (V.toList transactionPostings)) $ \(Located pl1 p1, mP2) -> do
    let Located _ accountName = postingAccountName p1
    let Located al1 account = postingAccount p1
    case M.lookup accountName accounts of
      Nothing -> undefined -- Undeclared account, should not happen.
      Just (Located _ acc) -> case accountType acc of
        AccountTypeExpenses | account >= Account.zero ->
          case mP2 of
            Nothing ->
              -- If this IS the VAT posting, it's fine.
              if accountName == vatInputVATExpensesAccountName
                then pure Nothing
                else validationTFailure $ VATErrorNoVATPosting tl pl1
            Just (Located pl2 p2) -> do
              let Located _ vatAccountName = postingAccountName p2
              if vatAccountName == vatInputVATExpensesAccountName
                then fmap Just $ do
                  let deductibleExpensePosting = pl1
                  let Located _ timestamp = transactionTimestamp
                  let deductibleExpenseTimestamp = timestamp
                  let day = Timestamp.toDay timestamp
                  let Located _ deductibleExpenseCurrency = postingCurrency p1
                  deductibleExpenseAmount <- requirePositive tl pl1 account
                  deductibleExpenseCHFAmount <- convertDaily al1 dailyPriceGraphs day deductibleExpenseCurrency chf deductibleExpenseAmount
                  deductibleExpenseDescription <- requireDescription transactionDescription
                  deductibleExpenseEvidence <- requireEvidence tl [reldir|deductions|] transactionAttachments
                  let Located _ deductibleExpenseVATCurrency = postingCurrency p2
                  let Located al vatAccount = postingAccount p2
                  deductibleExpenseVATAmount <- requirePositive tl pl2 vatAccount
                  deductibleExpenseVATCHFAmount <- convertDaily al dailyPriceGraphs day deductibleExpenseVATCurrency chf deductibleExpenseVATAmount
                  (percl, reportedVATRate) <- requirePercentageRate pl2 $ postingAmountRatio p2
                  deductibleExpenseVATRate <-
                    if deductibleExpenseVATCurrency == chf
                      then vatRateRatio <$> requireRatioVATRate tl percl reportedVATRate
                      else pure reportedVATRate -- No way to check if it's a foreign VAT rate
                  pure DeductibleExpense {..}
                else pure Nothing
        _ -> pure Nothing

consequtiveTups :: [a] -> [(a, Maybe a)]
consequtiveTups l =
  zip l (map Just (drop 1 l) ++ [Nothing])

simplifyDir :: Path Rel File -> Path Rel File
simplifyDir f =
  let pn = parent f
      dn = dirname pn
      fn = filename f
   in dn </> fn
