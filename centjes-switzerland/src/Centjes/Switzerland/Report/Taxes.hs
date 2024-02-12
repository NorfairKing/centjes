{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Switzerland.Report.Taxes
  ( TaxesInput (..),
    TaxesReport (..),
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
import Centjes.Switzerland.Reporter
import Centjes.Validation
import qualified Data.Map as M
import qualified Money.Amount as Amount
import qualified Money.QuantisationFactor as QuantisationFactor

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

  -- TODO fill in the actual revenue from the ledger
  let taxesReportSelfEmploymentRevenue = Amount.fromMinimalQuantisations 200

  pure TaxesReport {..}
