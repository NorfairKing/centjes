{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Centjes.Switzerland.Report.VAT
  ( VATReport (..),
    VATError (..),
    produceVATReport,
  )
where

import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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
import Money.Amount as Money (Amount (..))
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
    vatReportDomesticRevenue :: !Money.Amount
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
            == Just vatReportTotalRevenue
      ]

data VATError ann
  = VATErrorNoCHF
  | VATErrorNoEvidence !ann
  | VATErrorPositiveIncome !ann !ann !Money.Account
  | VATErrorDomesticIncomeNotCHF !(Currency ann)
  | VATErrorSum ![Money.Amount]
  | VATErrorAdd !Money.Amount !Money.Amount
  deriving (Show, Eq, Generic)

instance Validity ann => Validity (VATError ann)

instance ToReport (VATError SourceSpan) where
  toReport = \case
    VATErrorNoCHF -> Err Nothing "no CHF currency defined" [] []
    VATErrorNoEvidence _ -> Err Nothing "no evidence in transaction" [] []
    VATErrorPositiveIncome tl pl _ ->
      Err
        Nothing
        "Positive income amount"
        [ (toDiagnosePosition pl, Where "in this posting"),
          (toDiagnosePosition tl, Blank)
        ]
        []
    VATErrorDomesticIncomeNotCHF _ -> Err Nothing "domestic income was not in CHF" [] []
    VATErrorSum _ -> Err Nothing "could not sum amounts because the result would get too big" [] []
    VATErrorAdd _ _ -> Err Nothing "could not add amounts because the result wolud get too big" [] []

produceVATReport ::
  Eq ann =>
  Ledger ann ->
  Reporter (VATError ann) (VATReport ann)
produceVATReport Ledger {..} = do
  -- TODO make these configurable
  let vatReportName = "Tom Sydney Kerckhove"
  let vatReportQuarter = YearQuarter 2024 Q1
  let domesticIncomeAccountName = "income:domestic"
  let foreignIncomeAccountName = "income:foreign"

  let chfSymbol = CurrencySymbol "CHF"
  vatReportCHF <- case M.lookup chfSymbol ledgerCurrencies of
    Nothing -> validationTFailure VATErrorNoCHF
    Just lqf -> pure $ Currency chfSymbol lqf

  vatReportDomesticRevenue <- do
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
                    if accountName == domesticIncomeAccountName
                      then do
                        let Located _ currency = postingCurrency
                        when (currency /= vatReportCHF) $ validationTFailure $ VATErrorDomesticIncomeNotCHF currency
                        let Located _ account = postingAccount
                        amount <- requireNegative tl pl account
                        pure $ Just amount
                      else pure Nothing

              pure $ Just amounts
            else pure Nothing
    case Amount.sum amounts of
      Nothing -> validationTFailure $ VATErrorSum amounts
      Just a -> pure a

  vatReportForeignRevenue <- do
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
                    if accountName == foreignIncomeAccountName
                      then do
                        let Located _ _currency = postingCurrency
                        let Located _ account = postingAccount
                        amount <- requireNegative tl pl account
                        -- TODO do currency conversion on the apropriate day
                        pure $ Just amount
                      else pure Nothing

              pure $ Just amounts
            else pure Nothing
    case Amount.sum amounts of
      Nothing -> validationTFailure $ VATErrorSum amounts
      Just a -> pure a

  vatReportTotalRevenue <- case Amount.add vatReportDomesticRevenue vatReportForeignRevenue of
    Nothing -> validationTFailure $ VATErrorAdd vatReportDomesticRevenue vatReportForeignRevenue
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
        includeFile rf rf -- TODO subdir in tarball?
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
