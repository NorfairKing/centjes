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
    VATRate (..),
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
import Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
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
import qualified Money.Account as Account
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Numeric.Natural
import Path

-- TODO upstream this to validity-time
deriving instance Generic Quarter

instance Validity Quarter

data VATReport ann = VATReport
  { vatReportName :: !Text,
    vatReportQuarter :: !Quarter,
    vatReportCHF :: !(Currency ann),
    vatReportDomesticRevenues :: ![DomesticRevenue ann],
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
    vatReportTotalDomesticRevenue :: !Money.Amount,
    -- | 302
    --
    -- Leistungen zum Normalsatz 7.7%
    vatReportDomesticRevenue2023 :: !Money.Amount,
    vatReport2023StandardRateVATRevenue :: !Money.Amount,
    -- | 303
    --
    -- Leistungen zum Normalsatz 8.1%
    vatReportDomesticRevenue2024 :: !Money.Amount,
    vatReport2024StandardRateVATRevenue :: !Money.Amount,
    -- | 399
    --
    -- Total geschuldete Steuer (Ziff. 301 bis Ziff. 382)
    vatReportTotalVATRevenue :: !Money.Amount,
    -- | 405
    --
    -- Vorsteuer auf Investitionen und übrigem Betriebsaufwand
    vatReportPaidVAT :: !Money.Amount,
    -- | 500
    --
    -- Zu bezahlender Betrag
    vatReportPayable :: !Money.Account
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (VATReport ann) where
  validate vr@VATReport {..} =
    mconcat
      [ genericValidate vr,
        declare "The total domestic revenue is the total of the revenues" $
          Amount.sum (map domesticRevenueCHFAmount vatReportDomesticRevenues)
            == Just vatReportTotalDomesticRevenue,
        declare
          "The total revenue is the sum of domestic and foreign"
          $ Amount.add
            vatReportTotalDomesticRevenue
            vatReportForeignRevenue
            == Just vatReportTotalRevenue,
        declare "The total 2023 standard rate VAT revenue is the total of VAT amounts of the revenues" $
          Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2023Standard) . domesticRevenueVATRate) vatReportDomesticRevenues))
            == Just vatReport2023StandardRateVATRevenue,
        declare "The total 2024 standard rate VAT revenue is the total of VAT amounts of the revenues" $
          Amount.sum (map domesticRevenueVATCHFAmount (filter ((== VATRate2024Standard) . domesticRevenueVATRate) vatReportDomesticRevenues))
            == Just vatReport2024StandardRateVATRevenue,
        declare "The total vat is the sum of all the vat fields" $
          Amount.sum [vatReport2023StandardRateVATRevenue, vatReport2024StandardRateVATRevenue] == Just vatReportTotalVATRevenue,
        declare "The payable amount is the VAT revenue minus the paid VAT" $
          Account.subtract (Account.fromAmount vatReportTotalVATRevenue) (Account.fromAmount vatReportPaidVAT) == Just vatReportPayable
      ]

data DomesticRevenue ann = DomesticRevenue
  { domesticRevenueTimestamp :: !Timestamp,
    domesticRevenueDescription :: !Description,
    domesticRevenueAmount :: !Money.Amount,
    domesticRevenueCurrency :: !(Currency ann),
    domesticRevenueCHFAmount :: !Money.Amount,
    domesticRevenueVATAmount :: !Money.Amount,
    domesticRevenueVATCurrency :: !(Currency ann),
    domesticRevenueVATCHFAmount :: !Money.Amount,
    domesticRevenueVATRate :: !VATRate,
    -- | Evidence in tarball
    domesticRevenueEvidence :: !(NonEmpty (Path Rel File))
  }
  deriving (Show, Eq, Generic)

instance (Validity ann, Show ann, Ord ann) => Validity (DomesticRevenue ann)

data VATRate
  = -- | 7.7%
    VATRate2023Standard
  | -- | 8.1%
    VATRate2024Standard
  deriving (Show, Eq, Generic)

instance Validity VATRate

data VATError ann
  = VATErrorNoCHF
  | VATErrorNoDescription
  | VATErrorNoEvidence !ann
  | VATErrorCouldNotConvert !ann !(Currency ann) !(Currency ann) !Money.Amount
  | VATErrorPositiveIncome !ann !ann !Money.Account
  | VATErrorNegativeExpense !ann !ann !Money.Account
  | VATErrorNoVATPosting
  | VATErrorVATPostingNotVATAccount
  | VATErrorNoVATPercentage
  | VATErrorUnknownVATRate !ann !(Ratio Natural)
  | VATErrorSum ![Money.Amount]
  | VATErrorAdd !Money.Amount !Money.Amount
  | VATErrorSubtract !Money.Amount !Money.Amount
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (VATError ann)

instance ToReport (VATError SourceSpan) where
  toReport = \case
    VATErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    VATErrorNoDescription -> Err Nothing "no description" [] []
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
    VATErrorNoVATPercentage -> Err Nothing "VAT posting for domestic income did not have a percentage" [] []
    VATErrorUnknownVATRate pl _ ->
      Err
        Nothing
        "Unknown VAT rate"
        [ (toDiagnosePosition pl, This "in this percentage")
        ]
        []
    VATErrorSum _ -> Err Nothing "could not sum amounts because the result would get too big" [] []
    VATErrorAdd _ _ -> Err Nothing "could not add amounts because the result wolud get too big" [] []
    VATErrorSubtract _ _ -> Err Nothing "Could not subtract amounts because the result wolud get too big or too small" [] []

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
            let domesticRevenueTimestamp = timestamp
            domesticRevenueDescription <- case transactionDescription of
              Nothing -> validationTFailure VATErrorNoDescription
              Just (Located _ d) -> pure d
            domesticRevenueEvidence <- requireEvidence tl transactionAttachments

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
                          domesticRevenueVATRate <- requireVATRate (postingPercentage p2)
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

  vatReportForeignRevenue <- do
    amounts <- fmap (concat . catMaybes) $
      forM (V.toList ledgerTransactions) $
        \(Located tl Transaction {..}) -> do
          let Located _ timestamp = transactionTimestamp
          let day = Timestamp.toDay timestamp
          if dayInQuarter vatReportQuarter day
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

  vatReportTotalRevenue <- requireAddAmount vatReportTotalDomesticRevenue vatReportForeignRevenue

  vatReportTotalVATRevenue <-
    requireSumAmount
      [ vatReport2023StandardRateVATRevenue,
        vatReport2024StandardRateVATRevenue
      ]

  vatReportPaidVAT <- do
    amounts <- fmap (concat . catMaybes) $
      forM (V.toList ledgerTransactions) $
        \(Located tl Transaction {..}) -> do
          let Located _ timestamp = transactionTimestamp
          let day = Timestamp.toDay timestamp
          if dayInQuarter vatReportQuarter day
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

  vatReportPayable <- case Account.subtract (Account.fromAmount vatReportTotalVATRevenue) (Account.fromAmount vatReportPaidVAT) of
    Nothing -> validationTFailure $ VATErrorSubtract vatReportTotalRevenue vatReportPaidVAT
    Just a -> pure a

  pure VATReport {..}

dayInQuarter :: Quarter -> Day -> Bool
dayInQuarter quarter day =
  day >= periodFirstDay quarter && day <= periodLastDay quarter

requireEvidence ::
  ann ->
  Vector (GenLocated ann (Attachment ann)) ->
  Reporter (VATError ann) (NonEmpty (Path Rel File))
requireEvidence tl attachments =
  case NE.nonEmpty (V.toList attachments) of
    Nothing -> validationTFailure $ VATErrorNoEvidence tl
    Just ne ->
      forM ne $ \(Located _ (Attachment (Located _ rf))) -> do
        let pathInTarball = [reldir|income|] </> rf
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

requireVATRate :: Maybe (GenLocated ann (Percentage ann)) -> Reporter (VATError ann) VATRate
requireVATRate = \case
  Nothing -> validationTFailure VATErrorNoVATPercentage
  Just (Located pl (Percentage (Located _ r))) -> case r of
    0.077 -> pure VATRate2023Standard
    0.081 -> pure VATRate2024Standard
    _ -> validationTFailure $ VATErrorUnknownVATRate pl r
