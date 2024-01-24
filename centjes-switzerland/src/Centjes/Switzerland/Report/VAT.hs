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
    VATError (..),
    produceVATReport,
  )
where

import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import qualified Centjes.CurrencySymbol as CurrencySymbol
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Time.Calendar.Quarter
import Data.Validity
import Data.Validity.Time ()
import Data.Vector (Vector)
import qualified Data.Vector as V
import Error.Diagnose
import GHC.Generics (Generic (..))
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Path

-- TODO upstream this to validity-time
deriving instance Generic Quarter

instance Validity Quarter

data VATReport ann = VATReport
  { vatReportName :: !Text,
    vatReportQuarter :: !Quarter,
    vatReportCHF :: !(Currency ann),
    -- | 200
    --
    -- Total der vereinbarten bzw. vereinnahmten Entgelte, inkl.
    -- optierte Leistungen, Entgelte aus Übertragungen im
    -- Meldeverfahren sowie aus Leistungen im Ausland
    -- (weltweiter Umsatz)
    vatReportTotalRevenue :: !Money.Amount,
    -- | 221
    --
    -- Leistungen im Ausland (Ort der Leistung im Ausland)
    vatReportForeignRevenue :: !Money.Amount,
    -- | 299
    --
    -- Steuerbarer Gesamtumsatz (Ziff. 200 abzüglich Ziff. 289)
    vatReportDomesticRevenue :: !Money.Amount,
    -- 302 Leistungen zum Normalsatz 8.1%
    vatReportStandardRateVAT81PercentRevenue :: !Money.Amount,
    -- 399 Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)
    vatReportTotalVATRevenue :: !Money.Amount,
    -- 405 Vorsteuer auf Investitionen und übrigem Betriebsaufwand
    vatReportPaidVAT :: !Money.Amount
  }
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (VATReport ann) where
  validate vr@VATReport {..} =
    mconcat
      [ genericValidate vr,
        declare "The total revenue is the sum of domestic and foreign" $
          Amount.add
            vatReportDomesticRevenue
            vatReportForeignRevenue
            == Just vatReportTotalRevenue,
        declare "The total vat is the sum of all the vat fields" $
          Amount.sum [vatReportStandardRateVAT81PercentRevenue] == Just vatReportTotalVATRevenue
      ]

data VATError ann
  = VATErrorNoCHF
  | VATErrorNoEvidence !ann
  | VATErrorCouldNotConvert !ann !(Currency ann) !(Currency ann) !Money.Amount
  | VATErrorPositiveIncome !ann !ann !Money.Account
  | VATErrorNegativeExpense !ann !ann !Money.Account
  | VATErrorNoVATPosting
  | VATErrorVATPostingNotVATAccount
  | VATErrorSum ![Money.Amount]
  | VATErrorAdd !Money.Amount !Money.Amount
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (VATError ann)

instance ToReport (VATError SourceSpan) where
  toReport = \case
    VATErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    VATErrorNoEvidence _ -> Err Nothing "no evidence in transaction" [] []
    VATErrorCouldNotConvert al currencyFrom currencyTo _ ->
      let symbolFrom = currencySymbol currencyFrom
          symbolTo = currencySymbol currencyTo
       in Err
            Nothing
            ( unwords
                [ "could not convert from",
                  CurrencySymbol.toString symbolFrom,
                  "to",
                  CurrencySymbol.toString symbolTo
                ]
            )
            [(toDiagnosePosition al, This "this amount")]
            []
    VATErrorPositiveIncome tl pl _ ->
      Err
        Nothing
        "Positive income amount"
        [ (toDiagnosePosition pl, Where "in this posting"),
          (toDiagnosePosition tl, Blank)
        ]
        []
    VATErrorNegativeExpense tl pl _ ->
      Err
        Nothing
        "Negative expense amount"
        [ (toDiagnosePosition pl, Where "in this posting"),
          (toDiagnosePosition tl, Blank)
        ]
        []
    VATErrorNoVATPosting -> Err Nothing "No VAT posting for domestic income" [] []
    VATErrorVATPostingNotVATAccount -> Err Nothing "VAT posting for domestic income had unknown account name" [] []
    VATErrorSum _ -> Err Nothing "could not sum amounts because the result would get too big" [] []
    VATErrorAdd _ _ -> Err Nothing "could not add amounts because the result wolud get too big" [] []

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

  (vatReportDomesticRevenue, vatReportStandardRateVAT81PercentRevenue) <- do
    amountTups <- fmap (concat . catMaybes) $
      forM (V.toList ledgerTransactions) $
        \(Located tl Transaction {..}) -> do
          let Located _ timestamp = transactionTimestamp
          let day = Timestamp.toDay timestamp
          if day >= periodFirstDay vatReportQuarter && day <= periodLastDay vatReportQuarter
            then do
              _ <- requireEvidence tl transactionAttachments

              -- Every posting and the next
              let postingsTups =
                    let l = V.toList transactionPostings
                     in zip l (map Just (tail l) ++ [Nothing])

              amounts <- fmap catMaybes $
                forM postingsTups $
                  \(Located pl1 p1, mP2) -> do
                    let Located _ accountName = postingAccountName p1
                    if accountName == domesticIncomeAccountName
                      then fmap Just $ do
                        let Located _ currency = postingCurrency p1
                        let Located al1 account = postingAccount p1
                        amount <- requireNegative tl pl1 account
                        amount' <- convertDaily al1 dailyPriceGraphs day currency vatReportCHF amount

                        case mP2 of
                          Nothing -> validationTFailure VATErrorNoVATPosting
                          Just (Located pl2 p2) -> do
                            let Located al2 vatAccountName = postingAccountName p2
                            when (vatAccountName /= vatIncomeAccountName) $ validationTFailure VATErrorVATPostingNotVATAccount
                            let Located _ vatCurrency = postingCurrency p2
                            let Located _ vatAccount = postingAccount p2
                            -- TODO assert that the rate is 8.1%
                            vatAmount <- requireNegative tl pl2 vatAccount
                            vatAmount' <- convertDaily al2 dailyPriceGraphs day vatCurrency vatReportCHF vatAmount
                            pure (amount', vatAmount')
                      else pure Nothing

              pure $ Just amounts
            else pure Nothing

    let (amounts, vatAmounts) = unzip amountTups
    rA <- case Amount.sum amounts of
      Nothing -> validationTFailure $ VATErrorSum amounts
      Just a -> pure a
    vA <- case Amount.sum vatAmounts of
      Nothing -> validationTFailure $ VATErrorSum amounts
      Just a -> pure a
    pure (rA, vA)

  vatReportForeignRevenue <- do
    amounts <- fmap (concat . catMaybes) $
      forM (V.toList ledgerTransactions) $
        \(Located tl Transaction {..}) -> do
          let Located _ timestamp = transactionTimestamp
          let day = Timestamp.toDay timestamp
          if day >= periodFirstDay vatReportQuarter && day <= periodLastDay vatReportQuarter
            then do
              _ <- requireEvidence tl transactionAttachments
              -- TODO assert that the rate is one of the common ones

              amounts <- fmap catMaybes $
                forM (V.toList transactionPostings) $
                  \(Located pl Posting {..}) -> do
                    let Located _ accountName = postingAccountName
                    if accountName == foreignIncomeAccountName
                      then do
                        let Located _ currency = postingCurrency
                        let Located al account = postingAccount
                        amount <- requireNegative tl pl account
                        amount' <- convertDaily al dailyPriceGraphs day currency vatReportCHF amount
                        pure $ Just amount'
                      else pure Nothing

              pure $ Just amounts
            else pure Nothing
    case Amount.sum amounts of
      Nothing -> validationTFailure $ VATErrorSum amounts
      Just a -> pure a

  vatReportTotalRevenue <- case Amount.add vatReportDomesticRevenue vatReportForeignRevenue of
    Nothing -> validationTFailure $ VATErrorAdd vatReportDomesticRevenue vatReportForeignRevenue
    Just a -> pure a

  let vatReportTotalVATRevenue = vatReportStandardRateVAT81PercentRevenue

  vatReportPaidVAT <- do
    amounts <- fmap (concat . catMaybes) $
      forM (V.toList ledgerTransactions) $
        \(Located tl Transaction {..}) -> do
          let Located _ timestamp = transactionTimestamp
          let day = Timestamp.toDay timestamp
          if day >= periodFirstDay vatReportQuarter && day <= periodLastDay vatReportQuarter
            then do
              _ <- requireEvidence tl transactionAttachments

              amounts <- fmap catMaybes $
                forM (V.toList transactionPostings) $
                  \(Located pl Posting {..}) -> do
                    let Located _ accountName = postingAccountName
                    if accountName == vatExpensesAccountName
                      then do
                        let Located _ currency = postingCurrency
                        let Located al account = postingAccount
                        amount <- requirePositive tl pl account
                        amount' <- convertDaily al dailyPriceGraphs day currency vatReportCHF amount
                        pure $ Just amount'
                      else pure Nothing

              pure $ Just amounts
            else pure Nothing
    case Amount.sum amounts of
      Nothing -> validationTFailure $ VATErrorSum amounts
      Just a -> pure a

  pure VATReport {..}

requireEvidence ::
  ann ->
  Vector (GenLocated ann (Attachment ann)) ->
  Reporter (VATError ann) (NonEmpty (Path Rel File))
requireEvidence tl attachments =
  case NE.nonEmpty (V.toList attachments) of
    Nothing -> validationTFailure $ VATErrorNoEvidence tl
    Just ne ->
      forM ne $ \(Located _ (Attachment (Located _ rf))) -> do
        includeFile ([reldir|income|] </> rf) rf
        pure rf

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
