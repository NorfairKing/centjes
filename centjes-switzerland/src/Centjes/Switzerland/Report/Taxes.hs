{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time
import Data.Traversable
import qualified Data.Vector as V
import Money.Account as Money (Account (..))
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Money.MultiAccount (MultiAccount (..))
import qualified Money.MultiAccount as MultiAccount
import qualified Money.QuantisationFactor as QuantisationFactor
import Path

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

  -- Only use prices from the year
  let yearPrices =
        V.filter
          ( \(Located _ Price {..}) ->
              let Located _ ts = priceTimestamp
               in Timestamp.toDay ts <= endOfYear
          )
          ledgerPrices

  let memoisedPriceGraph = pricesToMemoisedPriceGraph yearPrices
  let dailyPriceGraphs = pricesToDailyPriceGraphs yearPrices

  let currenciesInPrices =
        M.fromList
          $ map
            ( \(Located _ Price {..}) ->
                let Located _ Currency {..} = priceCurrency
                 in (currencySymbol, currencyQuantisationFactor)
            )
          $ V.toList yearPrices

  let taxesReportConversionRates =
        M.fromList
          $ mapMaybe
            ( \(symbol, lqf) ->
                let from = Currency symbol lqf
                 in (,) from <$> MemoisedPriceGraph.lookup memoisedPriceGraph from taxesReportCHF
            )
          $ M.toList currenciesInPrices

  balanceReport <-
    liftValidation $
      mapValidationFailure TaxesErrorBalanceError $
        produceBalanceReport FilterAny (Just endOfYear) Nothing False ledger

  taxesReportAssetAccounts <- fmap catMaybes $ for (M.toList ledgerAccounts) $ \(an, Located al Account {..}) -> do
    let mUndeclaredTag = M.lookup taxesInputTagUndeclared accountTags
    case accountType of
      AccountTypeAssets -> case mUndeclaredTag of
        Just _ -> pure Nothing
        Nothing -> do
          let assetAccountName = an
          assetAccountBalances <- flip M.traverseWithKey (MultiAccount.unMultiAccount $ fromMaybe MultiAccount.zero $ M.lookup an $ balanceReportBalances balanceReport) $ \c b -> do
            positive <- requirePositive al b
            converted <-
              liftValidation $
                mapValidationFailure TaxesErrorConvertError $
                  convertMultiAccountToAccount (Just al) memoisedPriceGraph taxesReportCHF $
                    MultiAccount $
                      M.singleton c b
            positiveConverted <- requirePositive al converted
            pure (positive, positiveConverted)

          assetAccountConvertedBalance <-
            case Amount.sum $ M.map snd assetAccountBalances of
              Nothing -> validationTFailure TaxesErrorSum
              Just s -> pure s
          -- TODO assert that the balance is positive?
          -- this is checked anyway, we may use this in the type

          let attachments = map (locatedValue . attachmentPath . locatedValue) $ V.toList accountAttachments

          -- TODO This allows undeclared accounts with evidence, do we want that?
          case NE.nonEmpty attachments of
            Nothing -> validationTFailure $ TaxesErrorAssetAccountWithoutEvidence (Located al an)
            Just ne -> do
              assetAccountAttachments <- forM ne $ \rf -> do
                let fileInTarball = [reldir|assets|] </> simplifyDir rf
                includeFile fileInTarball rf
                pure fileInTarball
              pure $ Just AssetAccount {..}
      _ -> pure Nothing

  taxesReportTotalAssets <-
    case Amount.sum $ map assetAccountConvertedBalance taxesReportAssetAccounts of
      Nothing -> validationTFailure TaxesErrorSum
      Just s -> pure s

  taxesReportRevenues <- fmap concat $
    forM (V.toList ledgerTransactions) $
      \(Located tl Transaction {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        if dayInYear taxesReportYear day
          then fmap catMaybes $
            forM (V.toList transactionPostings) $ \(Located pl Posting {..}) -> do
              let Located _ accountName = postingAccountName
              case M.lookup accountName ledgerAccounts of
                Nothing -> error "TODO: error on undefined account"
                Just (Located al Account {..}) ->
                  if accountType == AccountTypeIncome
                    then do
                      let mUndeclaredTag = M.lookup taxesInputTagUndeclared accountTags
                      case mUndeclaredTag of
                        Just _ -> pure Nothing
                        Nothing -> do
                          let attachments = map (locatedValue . attachmentPath . locatedValue) $ V.toList transactionAttachments

                          case NE.nonEmpty attachments of
                            Nothing -> validationTFailure $ TaxesErrorRevenueWithoutEvidence tl pl (Located al accountName)
                            Just ne -> do
                              let revenueTimestamp = timestamp
                              revenueEvidence <- forM ne $ \rf -> do
                                let fileInTarball = [reldir|income|] </> simplifyDir rf
                                includeFile fileInTarball rf
                                pure fileInTarball

                              revenueDescription <- requireDescription transactionDescription

                              let Located _ revenueCurrency = postingCurrency
                              let Located al1 account = postingAccount

                              revenueAmount <- requireNegative tl pl account

                              revenueCHFAmount <- convertDaily al1 dailyPriceGraphs day revenueCurrency taxesReportCHF revenueAmount

                              pure $ Just Revenue {..}
                    else pure Nothing
          else pure []

  taxesReportTotalRevenues <-
    case Amount.sum $ map revenueCHFAmount taxesReportRevenues of
      Nothing -> validationTFailure TaxesErrorSum
      Just s -> pure s

  pure TaxesReport {..}

dayInYear :: Integer -> Day -> Bool
dayInYear year day =
  let (y, _, _) = toGregorian day
   in y == year

simplifyDir :: Path Rel File -> Path Rel File
simplifyDir f =
  let pn = parent f
      dn = dirname pn
      fn = filename f
   in dn </> fn

requireDescription ::
  Maybe (GenLocated ann Description) ->
  Reporter (TaxesError ann) Description
requireDescription = \case
  Nothing -> validationTFailure TaxesErrorNoDescription
  Just (Located _ d) -> pure d

requirePositive ::
  ann ->
  Money.Account ->
  Reporter (TaxesError ann) Money.Amount
requirePositive al account =
  case account of
    Money.Negative _ -> validationTFailure $ TaxesErrorNegativeAsset al account
    Money.Positive a -> pure a

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
  (Ord ann) =>
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
            let (mA, _) = Amount.convert RoundNearest qfFrom amount rate qfTo
            case mA of
              Nothing -> validationTFailure $ TaxesErrorCouldNotConvert al currencyFrom currencyTo amount
              Just convertedAmount -> pure convertedAmount
