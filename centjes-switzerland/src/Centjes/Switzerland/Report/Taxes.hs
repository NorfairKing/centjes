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
import qualified Centjes.Convert.MemoisedPriceGraph as MemoisedPriceGraph
import Centjes.Filter
import Centjes.Ledger
import Centjes.Location
import Centjes.Report.Balance
import Centjes.Switzerland.Report.Taxes.ETax
import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.Report.Taxes.Typst
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Traversable
import qualified Data.Vector as V
import qualified Money.MultiAccount as MultiAccount
import qualified Money.QuantisationFactor as QuantisationFactor

produceTaxesReport ::
  (Ord ann) =>
  TaxesInput ->
  Ledger ann ->
  Reporter (TaxesError ann) (TaxesReport ann)
produceTaxesReport TaxesInput {..} ledger@Ledger {..} = do
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

  let endOfYear = fromGregorian taxesReportYear 12 31

  let yearPrices =
        V.filter
          ( \(Located _ Price {..}) ->
              let Located _ ts = priceTimestamp
               in Timestamp.toDay ts <= endOfYear
          )
          ledgerPrices

  let memoisedPriceGraph = pricesToPriceGraph yearPrices

  let taxesReportConversionRates =
        M.fromList
          $ mapMaybe
            ( \(symbol, lqf) ->
                let from = Currency symbol lqf
                 in (,) from <$> MemoisedPriceGraph.lookup memoisedPriceGraph from taxesReportCHF
            )
          $ M.toList ledgerCurrencies

  balanceReport <-
    liftValidation $
      mapValidationFailure TaxesErrorBalanceError $
        produceBalanceReport FilterAny (Just endOfYear) Nothing False ledger

  taxesReportAssetAccounts <- fmap catMaybes $ for (M.toList ledgerAccounts) $ \(an, Located al Account {..}) -> do
    let mUndeclaredTag = M.lookup taxesInputTagUndeclared accountTags
    case accountType of
      AccountTypeAssets -> do
        let assetAccountName = an
        let assetAccountBalance = fromMaybe MultiAccount.zero $ M.lookup an $ balanceReportBalances balanceReport
        assetAccountConvertedBalance <-
          liftValidation $
            mapValidationFailure TaxesErrorConvertError $
              convertMultiAccountToAccount (Just al) memoisedPriceGraph taxesReportCHF assetAccountBalance

        -- TODO assert that the balance is positive?
        -- this is checked anyway, we may use this in the type

        let attachments = map (locatedValue . attachmentPath . locatedValue) $ V.toList accountAttachments

        -- TODO This allows undeclared accounts with evidence, do we want that?
        case NE.nonEmpty attachments of
          Nothing -> case mUndeclaredTag of
            Nothing -> validationTFailure $ TaxesErrorAssetAccountWithoutEvidence (Located al an)
            Just _ -> pure Nothing
          Just assetAccountAttachments -> pure $ Just AssetAccount {..}
      _ -> pure Nothing

  pure TaxesReport {..}
