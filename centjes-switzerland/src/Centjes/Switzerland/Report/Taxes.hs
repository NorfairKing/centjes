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
import Centjes.Switzerland.Report.Common
import Centjes.Switzerland.Report.Taxes.ETax
import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.Report.Taxes.Typst
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
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
produceTaxesReport taxesInput@TaxesInput {..} ledger@Ledger {..} = do
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
            positive <- requireAssetPositive al b
            converted <-
              liftValidation $
                mapValidationFailure TaxesErrorConvertError $
                  convertMultiAccountToAccount (Just al) memoisedPriceGraph taxesReportCHF $
                    MultiAccount $
                      M.singleton c b
            positiveConverted <- requireAssetPositive al converted
            pure (positive, positiveConverted)

          assetAccountConvertedBalance <- requireSum $ M.map snd assetAccountBalances

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

  taxesReportTotalAssets <- requireSum $ map assetAccountConvertedBalance taxesReportAssetAccounts

  let yearTransactions =
        V.filter
          ( \(Located _ Transaction {..}) ->
              let Located _ timestamp = transactionTimestamp
               in dayInYear taxesReportYear (Timestamp.toDay timestamp)
          )
          ledgerTransactions
  let forYearTransactions func = concat <$> mapM func yearTransactions

  let forMatchingPostings accountPredicate func = forYearTransactions $ \lt@(Located _ Transaction {..}) -> do
        fmap catMaybes $ forM (V.toList transactionPostings) $ \lp@(Located _ p@Posting {..}) -> do
          let Located _ accountName = postingAccountName
          case M.lookup accountName ledgerAccounts of
            Nothing -> error "TODO: error on undefined account"
            Just la@(Located _ a) ->
              if accountPredicate a p
                then func lt la lp
                else pure Nothing

  taxesReportRevenues <- forMatchingPostings
    (\Account {..} Posting {..} -> postingReal && accountType == AccountTypeIncome)
    $ \(Located tl Transaction {..}) (Located al Account {..}) (Located pl Posting {..}) -> do
      let mUndeclaredTag = M.lookup taxesInputTagUndeclared accountTags
      case mUndeclaredTag of
        Just _ -> pure Nothing
        Nothing -> do
          let attachments = map (locatedValue . attachmentPath . locatedValue) $ V.toList transactionAttachments
          case NE.nonEmpty attachments of
            Nothing -> do
              let Located _ accountName = postingAccountName
              validationTFailure $ TaxesErrorRevenueWithoutEvidence tl pl (Located al accountName)
            Just ne -> do
              let Located _ timestamp = transactionTimestamp
              let day = Timestamp.toDay timestamp
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

  taxesReportTotalRevenues <- requireSum $ map revenueCHFAmount taxesReportRevenues

  let forAccountsPostings accountNames =
        forMatchingPostings (\_ Posting {..} -> postingReal && S.member (locatedValue postingAccountName) accountNames)

  taxesReportThirdPillarContributions <- forAccountsPostings
    ( S.fromList
        [ taxesInputThirdPillarInsuranceExpensesAccount,
          taxesInputThirdPillarAssetsAccount
        ]
    )
    $ \(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) -> withDeductibleTag taxesInput tl lp transactionTags $ do
      let Located _ timestamp = transactionTimestamp
      let day = Timestamp.toDay timestamp
      thirdPillarContributionDescription <- requireDescription transactionDescription
      let Located al account = postingAccount
      thirdPillarContributionAmount <- requireExpensePositive al account
      let thirdPillarContributionTimestamp = timestamp
      let Located _ thirdPillarContributionCurrency = postingCurrency
      thirdPillarContributionCHFAmount <- convertDaily al dailyPriceGraphs day thirdPillarContributionCurrency taxesReportCHF thirdPillarContributionAmount
      ne <- requireNonEmptyEvidence tl transactionAttachments
      thirdPillarContributionEvidence <- forM ne $ \rf -> do
        let fileInTarball = [reldir|third-pillar|] </> filename rf
        includeFile fileInTarball rf
        pure fileInTarball
      pure ThirdPillarContribution {..}

  taxesReportTotalThirdPillarContributions <- requireSum $ map thirdPillarContributionCHFAmount taxesReportThirdPillarContributions

  let forAccountPostings a = forAccountsPostings (S.singleton a)
  let forDeductableExpense a func = forAccountPostings a $ \lt@(Located tl Transaction {..}) _ lp ->
        withDeductibleTag taxesInput tl lp transactionTags $
          func lt lp

  taxesReportInsuranceExpenses <- forAccountsPostings
    ( S.fromList
        [ taxesInputAccidentInsuranceExpenseAccount,
          taxesInputDailyAllowanceInsuranceExpenseAccount
        ]
    )
    $ \(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) -> withDeductibleTag taxesInput tl lp transactionTags $ do
      let Located _ timestamp = transactionTimestamp
      let day = Timestamp.toDay timestamp
      insuranceExpenseDescription <- requireDescription transactionDescription
      let Located al account = postingAccount
      insuranceExpenseAmount <- requireExpensePositive al account
      let insuranceExpenseTimestamp = timestamp
      let Located _ insuranceExpenseCurrency = postingCurrency
      insuranceExpenseCHFAmount <- convertDaily al dailyPriceGraphs day insuranceExpenseCurrency taxesReportCHF insuranceExpenseAmount
      insuranceExpenseEvidence <- forM (map (locatedValue . attachmentPath . locatedValue) $ V.toList transactionAttachments) $ \rf -> do
        let fileInTarball = [reldir|expenses/insurance|] </> filename rf
        includeFile fileInTarball rf
        pure fileInTarball
      pure InsuranceExpense {..}

  taxesReportTotalInsuranceExpenses <- requireSum $ map insuranceExpenseCHFAmount taxesReportInsuranceExpenses

  taxesReportHomeofficeExpenses <-
    forDeductableExpense taxesInputHomeofficeExpensesAccount $
      \(Located _ Transaction {..}) (Located _ Posting {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        homeofficeExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        homeofficeExpenseAmount <- requireExpensePositive al account
        let homeofficeExpenseTimestamp = timestamp
        let Located _ homeofficeExpenseCurrency = postingCurrency
        homeofficeExpenseCHFAmount <- convertDaily al dailyPriceGraphs day homeofficeExpenseCurrency taxesReportCHF homeofficeExpenseAmount
        homeofficeExpenseEvidence <- forM (map (locatedValue . attachmentPath . locatedValue) $ V.toList transactionAttachments) $ \rf -> do
          let fileInTarball = [reldir|expenses/homeoffice|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure HomeofficeExpense {..}

  taxesReportTotalHomeofficeExpenses <- requireSum $ map homeofficeExpenseCHFAmount taxesReportHomeofficeExpenses

  taxesReportElectricityExpenses <-
    forDeductableExpense taxesInputElectricityExpensesAccount $
      \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        electricityExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        electricityExpenseAmount <- requireExpensePositive al account
        let electricityExpenseTimestamp = timestamp
        let Located _ electricityExpenseCurrency = postingCurrency
        electricityExpenseCHFAmount <- convertDaily al dailyPriceGraphs day electricityExpenseCurrency taxesReportCHF electricityExpenseAmount
        ne <- requireNonEmptyEvidence tl transactionAttachments
        electricityExpenseEvidence <- forM ne $ \rf -> do
          let fileInTarball = [reldir|expenses/electricity|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure ElectricityExpense {..}

  taxesReportTotalElectricityExpenses <- requireSum $ map electricityExpenseCHFAmount taxesReportElectricityExpenses

  taxesReportPhoneExpenses <-
    forDeductableExpense taxesInputPhoneExpensesAccount $
      \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        phoneExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        phoneExpenseAmount <- requireExpensePositive al account
        let phoneExpenseTimestamp = timestamp
        let Located _ phoneExpenseCurrency = postingCurrency
        phoneExpenseCHFAmount <- convertDaily al dailyPriceGraphs day phoneExpenseCurrency taxesReportCHF phoneExpenseAmount
        ne <- requireNonEmptyEvidence tl transactionAttachments
        phoneExpenseEvidence <- forM ne $ \rf -> do
          let fileInTarball = [reldir|expenses/phone|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure PhoneExpense {..}

  taxesReportTotalPhoneExpenses <- requireSum $ map phoneExpenseCHFAmount taxesReportPhoneExpenses

  taxesReportTravelExpenses <-
    forDeductableExpense taxesInputTravelExpensesAccount $
      \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        travelExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        travelExpenseAmount <- requireExpensePositive al account
        let travelExpenseTimestamp = timestamp
        let Located _ travelExpenseCurrency = postingCurrency
        travelExpenseCHFAmount <- convertDaily al dailyPriceGraphs day travelExpenseCurrency taxesReportCHF travelExpenseAmount
        ne <- requireNonEmptyEvidence tl transactionAttachments
        travelExpenseEvidence <- forM ne $ \rf -> do
          let fileInTarball = [reldir|expenses/travel|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure TravelExpense {..}

  taxesReportTotalTravelExpenses <- requireSum $ map travelExpenseCHFAmount taxesReportTravelExpenses

  taxesReportInternetExpenses <-
    forDeductableExpense taxesInputInternetExpensesAccount $
      \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        internetExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        internetExpenseAmount <- requireExpensePositive al account
        let internetExpenseTimestamp = timestamp
        let Located _ internetExpenseCurrency = postingCurrency
        internetExpenseCHFAmount <- convertDaily al dailyPriceGraphs day internetExpenseCurrency taxesReportCHF internetExpenseAmount
        ne <- requireNonEmptyEvidence tl transactionAttachments
        internetExpenseEvidence <- forM ne $ \rf -> do
          let fileInTarball = [reldir|expenses/internet|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure InternetExpense {..}

  taxesReportTotalInternetExpenses <- requireSum $ map internetExpenseCHFAmount taxesReportInternetExpenses

  taxesReportHealthExpenses <-
    forDeductableExpense taxesInputHealthExpensesAccount $
      \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        healthExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        healthExpenseAmount <- requireExpensePositive al account
        let healthExpenseTimestamp = timestamp
        let Located _ healthExpenseCurrency = postingCurrency
        healthExpenseCHFAmount <- convertDaily al dailyPriceGraphs day healthExpenseCurrency taxesReportCHF healthExpenseAmount
        ne <- requireNonEmptyEvidence tl transactionAttachments
        healthExpenseEvidence <- forM ne $ \rf -> do
          let fileInTarball = [reldir|expenses/health-insurance|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure HealthExpense {..}

  taxesReportTotalHealthExpenses <- requireSum $ map healthExpenseCHFAmount taxesReportHealthExpenses

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

requireAssetPositive ::
  ann ->
  Money.Account ->
  Reporter (TaxesError ann) Money.Amount
requireAssetPositive al account =
  case account of
    Money.Negative _ -> validationTFailure $ TaxesErrorNegativeAsset al account
    Money.Positive a -> pure a

requireExpensePositive ::
  ann ->
  Money.Account ->
  Reporter (TaxesError ann) Money.Amount
requireExpensePositive al account =
  case account of
    Money.Negative _ -> validationTFailure $ TaxesErrorNegativeExpense al account
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

requireNonEmptyEvidence ::
  ann ->
  Vector (GenLocated ann (Attachment ann)) ->
  Reporter (TaxesError ann) (NonEmpty (Path Rel File))
requireNonEmptyEvidence tl attachments =
  case NE.nonEmpty (V.toList attachments) of
    Nothing -> validationTFailure $ TaxesErrorNoEvidence tl
    Just ne -> forM ne $ \(Located _ (Attachment (Located _ rf))) -> pure rf

requireSum :: (Foldable f) => f Money.Amount -> Reporter (TaxesError ann) Money.Amount
requireSum amounts =
  case Amount.sum amounts of
    Nothing -> validationTFailure TaxesErrorSum
    Just s -> pure s

withDeductibleTag ::
  TaxesInput ->
  ann ->
  GenLocated ann (Posting ann) ->
  Map Tag ann ->
  Reporter (TaxesError ann) a ->
  Reporter (TaxesError ann) (Maybe a)
withDeductibleTag TaxesInput {..} tl lp tags continue = do
  let mTagDeductible = M.lookup taxesInputTagDeductible tags
  let mTagNotDeductible = M.lookup taxesInputTagNotDeductible tags
  let mTagTaxDeductible = M.lookup taxesInputTagTaxDeductible tags
  let mTagNotTaxDeductible = M.lookup taxesInputTagNotTaxDeductible tags
  case decideDeductible mTagDeductible mTagNotDeductible mTagTaxDeductible mTagNotTaxDeductible of
    DefinitelyDeductible _ -> Just <$> continue
    DefinitelyNotDeductible _ -> pure Nothing
    Undeclared -> validationTFailure $ TaxesErrorUntaggedExpenses tl lp
    RedundantlyDeclared t1l t2l -> validationTFailure $ TaxesErrorRedundantlyDeclared tl t1l t2l
    AmbiguouslyDeclared tyl tnl -> validationTFailure $ TaxesErrorDeductibleAndNotDeductible tl tyl tnl
