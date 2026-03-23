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
import Centjes.Report.EvaluatedLedger
import Centjes.Switzerland.Report.Common
import Centjes.Switzerland.Report.Taxes.ETax
import Centjes.Switzerland.Report.Taxes.Types
import Centjes.Switzerland.Report.Taxes.Typst
import Centjes.Switzerland.Reporter
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.Either
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import qualified Data.Set as S
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Money.Account as Money
import Money.Amount as Money (Amount (..), Rounding (..))
import qualified Money.Amount as Amount
import Money.MultiAccount (MultiAccount (..))
import qualified Money.MultiAccount as MultiAccount
import qualified Money.QuantisationFactor as QuantisationFactor
import Numeric.Natural
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

  evaluatedLedger <-
    liftValidation $
      mapValidationFailure TaxesErrorEvaluatedLedger $
        produceEvaluatedLedger ledger
  balanceReport <-
    liftValidation $
      mapValidationFailure TaxesErrorBalanceError $
        produceBalanceReportFromEvaluatedLedger FilterAny (Just endOfYear) Nothing False evaluatedLedger

  let endOfPreviousYear = fromGregorian (taxesReportYear - 1) 12 31
  previousYearBalanceReport <-
    liftValidation $
      mapValidationFailure TaxesErrorBalanceError $
        produceBalanceReportFromEvaluatedLedger FilterAny (Just endOfPreviousYear) Nothing False evaluatedLedger

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

  let forAccountsPostings accountNames =
        forMatchingPostings (\_ Posting {..} -> postingReal && S.member (locatedValue postingAccountName) accountNames)

  let forAccountPostings a = forAccountsPostings (S.singleton a)

  childrenCostsDaycare <- forAccountPostings taxesInputDaycareAccount $ \(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) ->
    withDeductibleTag taxesInput tl lp transactionTags $ do
      let Located _ timestamp = transactionTimestamp
      let day = Timestamp.toDay timestamp
      daycareExpenseDescription <- requireDescription transactionDescription
      let Located al account = postingAccount
      daycareExpenseAmount <- requireExpensePositive al account
      let daycareExpenseTimestamp = timestamp
      let Located _ daycareExpenseCurrency = postingCurrency
      daycareExpenseCHFAmount <- convertDaily al dailyPriceGraphs day daycareExpenseCurrency taxesReportCHF daycareExpenseAmount
      ne <- requireNonEmptyEvidence tl transactionAttachments
      daycareExpenseEvidence <- forM ne $ \rf -> do
        let fileInTarball = [reldir|expenses/daycare|] </> filename rf
        includeFile fileInTarball rf
        pure fileInTarball
      pure DaycareExpense {..}

  childrenCostsTotalDaycare <- requireSum $ map daycareExpenseCHFAmount childrenCostsDaycare

  let taxesReportChildrenCosts = ChildrenCosts {..}

  taxesReportEducationExpenses <- forAccountPostings taxesInputEducationAccount $ \(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) ->
    withDeductibleTag taxesInput tl lp transactionTags $ do
      let Located _ timestamp = transactionTimestamp
      let day = Timestamp.toDay timestamp
      educationExpenseDescription <- requireDescription transactionDescription
      let Located al account = postingAccount
      educationExpenseAmount <- requireExpensePositive al account
      let educationExpenseTimestamp = timestamp
      let Located _ educationExpenseCurrency = postingCurrency
      educationExpenseCHFAmount <- convertDaily al dailyPriceGraphs day educationExpenseCurrency taxesReportCHF educationExpenseAmount
      ne <- requireNonEmptyEvidence tl transactionAttachments
      educationExpenseEvidence <- forM ne $ \rf -> do
        let fileInTarball = [reldir|expenses/education|] </> filename rf
        includeFile fileInTarball rf
        pure fileInTarball
      pure EducationExpense {..}

  taxesReportTotalEducationExpenses <- requireSum $ map educationExpenseCHFAmount taxesReportEducationExpenses

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

  let makePrivateExpense (Located _ Transaction {..}) (Located _ Posting {..}) = do
        let Located _ timestamp = transactionTimestamp
        let day = Timestamp.toDay timestamp
        privateExpenseDescription <- requireDescription transactionDescription
        let Located al account = postingAccount
        privateExpenseAmount <- requireExpensePositive al account
        let privateExpenseTimestamp = timestamp
        let Located _ privateExpenseCurrency = postingCurrency
        privateExpenseCHFAmount <- convertDaily al dailyPriceGraphs day privateExpenseCurrency taxesReportCHF privateExpenseAmount
        privateExpenseEvidence <- forM (map (locatedValue . attachmentPath . locatedValue) $ V.toList transactionAttachments) $ \rf -> do
          let fileInTarball = [reldir|expenses/private|] </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure PrivateExpense {..}
  let forDeductableExpensePartitioned PartitionedExpenseAccounts {..} businessFunc = do
        eithers <- forAccountsPostings (S.fromList [partitionedExpenseAccountsBusiness, partitionedExpenseAccountsPrivate]) $ \lt@(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) ->
          let accountName = locatedValue postingAccountName
           in if accountName == partitionedExpenseAccountsPrivate
                then Just . Left <$> makePrivateExpense lt lp
                else Just <$> withDeductibleTagEither taxesInput tl lp transactionTags (businessFunc lt lp) (makePrivateExpense lt lp)
        pure $ partitionEithers eithers

  taxesReportInsuranceExpenses <-
    makePartitionedExpenses insuranceExpenseCHFAmount
      . partitionEithers
      =<< forAccountsPostings
        ( S.fromList
            [ taxesInputAccidentInsuranceExpenseAccount,
              taxesInputDailyAllowanceInsuranceExpenseAccount
            ]
        )
        ( \lt@(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) ->
            Just
              <$> withDeductibleTagEither
                taxesInput
                tl
                lp
                transactionTags
                ( do
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
                )
                (makePrivateExpense lt lp)
        )

  taxesReportHomeofficeExpenses <-
    makePartitionedExpenses homeofficeExpenseCHFAmount
      =<< forDeductableExpensePartitioned
        taxesInputHomeofficeExpenseAccounts
        ( \(Located _ Transaction {..}) (Located _ Posting {..}) -> do
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
        )

  taxesReportElectricityExpenses <-
    makePartitionedExpenses electricityExpenseCHFAmount
      =<< forDeductableExpensePartitioned
        taxesInputElectricityExpenseAccounts
        ( \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
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
        )

  taxesReportPhoneExpenses <-
    makePartitionedExpenses phoneExpenseCHFAmount
      =<< forDeductableExpensePartitioned
        taxesInputPhoneExpenseAccounts
        ( \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
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
        )

  taxesReportTravelExpenses <-
    makePartitionedExpenses travelExpenseCHFAmount
      =<< forDeductableExpensePartitioned
        taxesInputTravelExpenseAccounts
        ( \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
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
        )

  taxesReportInternetExpenses <-
    makePartitionedExpenses internetExpenseCHFAmount
      =<< forDeductableExpensePartitioned
        taxesInputInternetExpenseAccounts
        ( \(Located tl Transaction {..}) (Located _ Posting {..}) -> do
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
        )

  let makeHealthExpenses account tarballSubdir =
        forAccountPostings account $ \(Located tl Transaction {..}) _ lp@(Located _ Posting {..}) ->
          withDeductibleTag taxesInput tl lp transactionTags $ do
            let Located _ timestamp = transactionTimestamp
            let day = Timestamp.toDay timestamp
            healthExpenseDescription <- requireDescription transactionDescription
            let Located al account' = postingAccount
            healthExpenseAmount <- requireExpensePositive al account'
            let healthExpenseTimestamp = timestamp
            let Located _ healthExpenseCurrency = postingCurrency
            healthExpenseCHFAmount <- convertDaily al dailyPriceGraphs day healthExpenseCurrency taxesReportCHF healthExpenseAmount
            ne <- requireNonEmptyEvidence tl transactionAttachments
            healthExpenseEvidence <- forM ne $ \rf -> do
              let fileInTarball = tarballSubdir </> filename rf
              includeFile fileInTarball rf
              pure fileInTarball
            pure HealthExpense {..}

  healthCostsInsurancePremiums <- makeHealthExpenses taxesInputHealthInsurancePremiumsAccount [reldir|expenses/health/insurance-premiums|]
  healthCostsTotalInsurancePremiums <- requireSum $ map healthExpenseCHFAmount healthCostsInsurancePremiums

  healthCostsOther <- makeHealthExpenses taxesInputHealthOtherAccount [reldir|expenses/health/other|]
  healthCostsTotalOther <- requireSum $ map healthExpenseCHFAmount healthCostsOther

  healthCostsDentist <- makeHealthExpenses taxesInputHealthDentistAccount [reldir|expenses/health/dentist|]
  healthCostsTotalDentist <- requireSum $ map healthExpenseCHFAmount healthCostsDentist

  healthCostsDoctor <- makeHealthExpenses taxesInputHealthDoctorAccount [reldir|expenses/health/doctor|]
  healthCostsTotalDoctor <- requireSum $ map healthExpenseCHFAmount healthCostsDoctor

  healthCostsHospital <- makeHealthExpenses taxesInputHealthHospitalAccount [reldir|expenses/health/hospital|]
  healthCostsTotalHospital <- requireSum $ map healthExpenseCHFAmount healthCostsHospital

  healthCostsTherapy <- makeHealthExpenses taxesInputHealthTherapyAccount [reldir|expenses/health/therapy|]
  healthCostsTotalTherapy <- requireSum $ map healthExpenseCHFAmount healthCostsTherapy

  let taxesReportHealthCosts = HealthCosts {..}

  taxesReportMovables <-
    produceDepreciationSchedule
      taxesInput
      taxesInputMovablesAssetsAccount
      taxesInputMovablesExpensesAccount
      taxesInputMovablesDepreciationRate
      dailyPriceGraphs
      taxesReportCHF
      ledgerAccounts
      previousYearBalanceReport
      yearTransactions
      [reldir|depreciation/movables|]

  taxesReportMachinery <-
    produceDepreciationSchedule
      taxesInput
      taxesInputMachineryAssetsAccount
      taxesInputMachineryExpensesAccount
      taxesInputMachineryDepreciationRate
      dailyPriceGraphs
      taxesReportCHF
      ledgerAccounts
      previousYearBalanceReport
      yearTransactions
      [reldir|depreciation/machinery|]

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

makePartitionedExpenses ::
  (a -> Money.Amount) ->
  ([PrivateExpense ann], [a]) ->
  Reporter (TaxesError ann) (PartitionedExpenses a ann)
makePartitionedExpenses businessSumAccessor (privateExpenses, businessExpenses) = do
  partitionedExpensesTotalBusinessExpenses <- requireSum $ map businessSumAccessor businessExpenses
  partitionedExpensesTotalPrivateExpenses <- requireSum $ map privateExpenseCHFAmount privateExpenses
  let partitionedExpensesBusinessExpenses = businessExpenses
  let partitionedExpensesPrivateExpenses = privateExpenses
  pure PartitionedExpenses {..}

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

-- | Like 'withDeductibleTag' but returns 'Right' for deductible and 'Left' for not-deductible
-- instead of discarding not-deductible expenses.
withDeductibleTagEither ::
  TaxesInput ->
  ann ->
  GenLocated ann (Posting ann) ->
  Map Tag ann ->
  Reporter (TaxesError ann) a ->
  Reporter (TaxesError ann) b ->
  Reporter (TaxesError ann) (Either b a)
withDeductibleTagEither TaxesInput {..} tl lp tags businessContinue privateContinue = do
  let mTagDeductible = M.lookup taxesInputTagDeductible tags
  let mTagNotDeductible = M.lookup taxesInputTagNotDeductible tags
  let mTagTaxDeductible = M.lookup taxesInputTagTaxDeductible tags
  let mTagNotTaxDeductible = M.lookup taxesInputTagNotTaxDeductible tags
  case decideDeductible mTagDeductible mTagNotDeductible mTagTaxDeductible mTagNotTaxDeductible of
    DefinitelyDeductible _ -> Right <$> businessContinue
    DefinitelyNotDeductible _ -> Left <$> privateContinue
    Undeclared -> validationTFailure $ TaxesErrorUntaggedExpenses tl lp
    RedundantlyDeclared t1l t2l -> validationTFailure $ TaxesErrorRedundantlyDeclared tl t1l t2l
    AmbiguouslyDeclared tyl tnl -> validationTFailure $ TaxesErrorDeductibleAndNotDeductible tl tyl tnl

produceDepreciationSchedule ::
  (Ord ann) =>
  TaxesInput ->
  AccountName ->
  AccountName ->
  Ratio Natural ->
  Map Day (MemoisedPriceGraph (Currency ann)) ->
  Currency ann ->
  Map AccountName (GenLocated ann (Account ann)) ->
  BalanceReport ann ->
  Vector (GenLocated ann (Transaction ann)) ->
  Path Rel Dir ->
  Reporter (TaxesError ann) (DepreciationSchedule ann)
produceDepreciationSchedule taxesInput assetAccount expenseAccount depreciationScheduleDepreciationRate dailyPriceGraphs chf accounts previousYearBalanceReport yearTransactions tarballDir = do
  let endOfPreviousYear = fromGregorian (taxesInputYear taxesInput - 1) 12 31

  (accountLocation, depreciationScheduleOpeningBalanceEvidence) <- case M.lookup assetAccount accounts of
    Nothing -> validationTFailure $ TaxesErrorDepreciationAccountNotDeclared assetAccount
    Just (Located al Account {..}) -> do
      let attachments = map (locatedValue . attachmentPath . locatedValue) $ V.toList accountAttachments
      case NE.nonEmpty attachments of
        Nothing -> validationTFailure $ TaxesErrorAssetAccountWithoutEvidence (Located al assetAccount)
        Just ne -> do
          evidence <- forM ne $ \rf -> do
            let fileInTarball = tarballDir </> simplifyDir rf
            includeFile fileInTarball rf
            pure fileInTarball
          pure (al, evidence)

  depreciationScheduleOpeningBalance <- case M.lookup assetAccount $ balanceReportBalances previousYearBalanceReport of
    Nothing -> validationTFailure $ TaxesErrorDepreciationNoOpeningBalance accountLocation assetAccount (taxesInputYear taxesInput)
    Just ma -> convertBalance accountLocation endOfPreviousYear ma

  depreciationSchedulePurchases <- fmap (concatMap catMaybes) $ forM (V.toList yearTransactions) $ \(Located tl Transaction {..}) -> do
    let matchingPostings =
          filter
            ( \(Located _ Posting {..}) ->
                locatedValue postingAccountName == expenseAccount && postingReal
            )
            (V.toList transactionPostings)
    forM matchingPostings $ \lp@(Located _ Posting {..}) ->
      withDeductibleTag taxesInput tl lp transactionTags $ do
        let Located al account = postingAccount
        positive <- requireExpensePositive al account
        depreciationPurchaseDescription <- requireDescription transactionDescription
        let depreciationPurchaseTimestamp = locatedValue transactionTimestamp
        let Located _ purchaseCurrency = postingCurrency
        let day = Timestamp.toDay depreciationPurchaseTimestamp
        depreciationPurchaseAmount <-
          convertDaily al dailyPriceGraphs day purchaseCurrency chf positive
        ne <- requireNonEmptyEvidence tl transactionAttachments
        depreciationPurchaseEvidence <- forM ne $ \rf -> do
          let fileInTarball = tarballDir </> filename rf
          includeFile fileInTarball rf
          pure fileInTarball
        pure DepreciationPurchase {..}

  depreciationScheduleTotalPurchases <- requireSum $ map depreciationPurchaseAmount depreciationSchedulePurchases

  openingPlusPurchases <-
    case Amount.add depreciationScheduleOpeningBalance depreciationScheduleTotalPurchases of
      Nothing -> validationTFailure TaxesErrorDepreciationAdditionOverflow
      Just s -> pure s

  depreciationScheduleDepreciation <-
    case fst $ Amount.fraction RoundNearest openingPlusPurchases depreciationScheduleDepreciationRate of
      Nothing -> validationTFailure TaxesErrorDepreciationFractionError
      Just d -> pure d
  depreciationScheduleClosingBalance <-
    case Amount.subtract openingPlusPurchases depreciationScheduleDepreciation of
      Nothing -> validationTFailure TaxesErrorDepreciationSubtractionOverflow
      Just c -> pure c

  pure DepreciationSchedule {..}
  where
    convertBalance al day ma = do
      converted <- flip M.traverseWithKey (MultiAccount.unMultiAccount ma) $ \currency account -> do
        amount <- case account of
          Money.Positive a -> pure a
          Money.Negative _ -> validationTFailure $ TaxesErrorDepreciationNegativeBalance al assetAccount
        convertDaily al dailyPriceGraphs day currency chf amount
      requireSum $ M.elems converted
