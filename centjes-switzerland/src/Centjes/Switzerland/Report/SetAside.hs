{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | How much money has been set aside for a year's taxes, compared to how
-- much those taxes will be.
--
-- SVA and income taxes cost a share of the year's revenue.
-- Wealth tax does not depend on income, so it is computed from the taxable
-- wealth instead.
-- Virtual postings never count, so nothing in the ledger has to reserve
-- money for taxes.
module Centjes.Switzerland.Report.SetAside
  ( module Centjes.Switzerland.Report.SetAside.Types,
    produceSetAsideReport,
    shareOfRevenue,
    roundUpToHundreds,
    zurichMarriedWealthTariff,
    simpleWealthTax,
    zurichCantonalSteuerfuss,
    zurichMunicipalSteuerfuss,
  )
where

import Centjes.AccountType
import Centjes.Convert
import Centjes.Convert.MemoisedPriceGraph (MemoisedPriceGraph)
import Centjes.Ledger
import Centjes.Location
import Centjes.Switzerland.Report.Common
import Centjes.Switzerland.Report.SetAside.Types
import qualified Centjes.Timestamp as Timestamp
import Centjes.Validation
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import qualified Money.Account as Account
import qualified Money.Amount as Amount
import qualified Money.MultiAccount as MultiAccount
import qualified Money.QuantisationFactor as QuantisationFactor

-- | Today is only used to value wealth while the year still runs.
produceSetAsideReport ::
  forall ann.
  (Ord ann) =>
  SetAsideInput ->
  Day ->
  SetAsidePrices ann ->
  Ledger ann ->
  Validation (SetAsideError ann) SetAsideReport
produceSetAsideReport SetAsideInput {..} today prices Ledger {..} = do
  chf <- case M.lookup (CurrencySymbol "CHF") ledgerCurrencies of
    Nothing -> validationFailure SetAsideErrorNoCHF
    Just lqf@(Located _ qf) ->
      if Just qf == QuantisationFactor.fromWord32 100
        then pure $ Currency (CurrencySymbol "CHF") lqf
        else validationFailure $ SetAsideErrorWrongCHF lqf

  let mentionedAccounts :: [AccountName]
      mentionedAccounts =
        concatMap
          setAsideTaxReserveAccounts
          [setAsideInputSocialSecurity, setAsideInputCantonal, setAsideInputFederal]
          ++ M.keys (setAsideWealthOwnedShares setAsideInputWealth)
          ++ setAsideWealthReserveAccounts setAsideInputWealth
  forM_ mentionedAccounts $ \an ->
    unless (M.member an ledgerAccounts) $
      validationFailure $
        SetAsideErrorUndeclaredAccount an

  let valuationDay :: Day
      valuationDay = min today (fromGregorian setAsideInputYear 12 31)
  (setAsideReportWealthTaxBasis, wealthTaxRows) <-
    computeWealthTax chf setAsideInputYear valuationDay setAsideInputWealth prices ledgerAccounts ledgerTransactions

  -- Income in another currency is taxed at the rate of the day it came in,
  -- and the ESTV's daily rates are the ones the tax declaration uses.
  let officialDailyPriceGraphs = pricesToDailyPriceGraphs (setAsidePricesOfficial prices)
  let realPostings :: [(Day, Posting ann)]
      realPostings =
        [ (Timestamp.toDay timestamp, posting)
        | Located _ Transaction {..} <- V.toList ledgerTransactions,
          let Located _ timestamp = transactionTimestamp,
          Located _ posting <- V.toList transactionPostings,
          postingReal posting
        ]
  let yearPostings :: [(Day, Posting ann)]
      yearPostings = filter (dayInYear setAsideInputYear . fst) realPostings
  -- Money set aside in earlier years still counts, as long as it has not
  -- been spent.
  let reserveBalance :: AccountName -> Validation (SetAsideError ann) Account.Account
      reserveBalance an =
        sumPostingsInCHF
          chf
          officialDailyPriceGraphs
          [(day, p) | (day, p) <- realPostings, day <= valuationDay, locatedValue (postingAccountName p) == an]
  -- Revenue is income that the taxes report declares.
  let isRevenue :: AccountName -> Bool
      isRevenue an = case M.lookup an ledgerAccounts of
        Just (Located _ Account {..}) ->
          accountType == AccountTypeIncome && not (M.member setAsideInputTagUndeclared accountTags)
        Nothing -> False
  income <-
    sumPostingsInCHF
      chf
      officialDailyPriceGraphs
      [(day, p) | (day, p) <- yearPostings, isRevenue (locatedValue (postingAccountName p))]
  -- Income is booked as negative.
  let setAsideReportRevenue = negateAccount income
  let line :: [SetAsideRow] -> [AccountName] -> Validation (SetAsideError ann) SetAsideLine
      line owedRows reserveAccounts = do
        reserveRows <- forM reserveAccounts $ \an -> SetAsideRowReserve an <$> reserveBalance an
        let setAsideLineRows = owedRows ++ reserveRows
        setAsideLineSurplus <- requireSum $ map setAsideRowAmount setAsideLineRows
        pure SetAsideLine {..}
  let taxLine :: AccountName -> SetAsideTax -> Validation (SetAsideError ann) SetAsideLine
      taxLine liability SetAsideTax {..} = do
        share <- maybe (validationFailure SetAsideErrorOverflow) pure $ shareOfRevenue setAsideTaxRate setAsideReportRevenue
        line [SetAsideRowRevenueShare liability setAsideTaxRate (negateAccount share)] setAsideTaxReserveAccounts
  setAsideReportWealthTax <- line wealthTaxRows (setAsideWealthReserveAccounts setAsideInputWealth)
  setAsideReportSocialSecurity <- taxLine "liabilities:taxes:sva" setAsideInputSocialSecurity
  setAsideReportCantonal <- taxLine "liabilities:taxes:cantonal" setAsideInputCantonal
  setAsideReportFederal <- taxLine "liabilities:taxes:federal" setAsideInputFederal

  let taxes :: [(SetAsideActionKind, Text, [AccountName], SetAsideLine)]
      taxes =
        [ (SetAsideActionSave, "Wealth tax", setAsideWealthReserveAccounts setAsideInputWealth, setAsideReportWealthTax),
          (SetAsideActionSave, "SVA", setAsideTaxReserveAccounts setAsideInputSocialSecurity, setAsideReportSocialSecurity),
          (SetAsideActionPrepay, "Cantonal", setAsideTaxReserveAccounts setAsideInputCantonal, setAsideReportCantonal),
          (SetAsideActionSave, "Federal", setAsideTaxReserveAccounts setAsideInputFederal, setAsideReportFederal)
        ]
  setAsideReportActions <- fmap catMaybes $
    forM taxes $ \(setAsideActionKind, setAsideActionTax, reserveAccounts, SetAsideLine {..}) ->
      case setAsideLineSurplus of
        Account.Positive _ -> pure Nothing
        Account.Negative setAsideActionShortfall -> do
          setAsideActionAmount <- maybe (validationFailure SetAsideErrorOverflow) pure $ roundUpToHundreds setAsideActionShortfall
          let setAsideActionAccount = listToMaybe reserveAccounts
          pure $ Just SetAsideAction {..}
  let setAsideReportSurpluses =
        [ SetAsideSurplus {..}
        | (_, setAsideSurplusTax, _, SetAsideLine {..}) <- taxes,
          Account.Positive setAsideSurplusAmount <- [setAsideLineSurplus],
          setAsideSurplusAmount /= Amount.zero
        ]
  let setAsideReportYear = setAsideInputYear
  pure SetAsideReport {..}

-- | A share of revenue, rounded to the nearest rappen.
shareOfRevenue :: Scientific -> Account.Account -> Maybe Account.Account
shareOfRevenue rate revenue =
  Account.fromMinimalQuantisations $
    round (toRational rate * fromInteger (Account.toMinimalQuantisations revenue))

-- | Round CHF up to whole hundreds, to have a round amount to transfer that
-- is at least enough.
roundUpToHundreds :: Amount.Amount -> Maybe Amount.Amount
roundUpToHundreds amount =
  let rappen :: Integer
      rappen = toInteger (Amount.toMinimalQuantisations amount)
      hundred :: Integer
      hundred = 100 * 100
      rounded :: Integer
      rounded = (rappen + hundred - 1) `div` hundred * hundred
   in if rounded > toInteger (maxBound :: Word64)
        then Nothing
        else Just (Amount.fromMinimalQuantisations (fromInteger rounded))

-- | Negate, without turning zero into a negative zero that renders as -0.00.
negateAccount :: Account.Account -> Account.Account
negateAccount account =
  if account == Account.zero
    then Account.zero
    else Account.negate account

requireSum :: [Account.Account] -> Validation (SetAsideError ann) Account.Account
requireSum = maybe (validationFailure SetAsideErrorOverflow) pure . Account.sum

requireRappen :: Integer -> Validation (SetAsideError ann) Account.Account
requireRappen = maybe (validationFailure SetAsideErrorOverflow) pure . Account.fromMinimalQuantisations

-- | Sum postings, each converted to CHF at the latest
-- official rate on or before its own day.
sumPostingsInCHF ::
  (Ord ann) =>
  Currency ann ->
  Map Day (MemoisedPriceGraph (Commodity ann)) ->
  [(Day, Posting ann)] ->
  Validation (SetAsideError ann) Account.Account
sumPostingsInCHF chf dailyPriceGraphs postings = do
  converted <- forM postings $ \(day, p@Posting {..}) -> do
    let Located l account = postingAccount
    let Located _ commodity = postingCommodity p
    if commodityCurrency commodity == chf
      then pure account
      else case M.lookupLE day dailyPriceGraphs of
        Nothing -> validationFailure $ SetAsideErrorNoOfficialRateBefore l day
        Just (_, graph) ->
          mapValidationFailure SetAsideErrorConvert $
            convertMultiAccountToAccount (Just l) graph chf $
              MultiAccount.MultiAccount (M.singleton commodity account)
  requireSum converted

-- | The basis of the wealth tax, and the rows of what the canton and the
-- municipality charge on it.
--
-- Taxable wealth is what real assets and liabilities hold on the valuation
-- day, without the excluded accounts and with only the owned percentage of
-- shared ones.
--
-- Wealth is valued at the end of the year with the ESTV's prices: its
-- exchange rates and the year-end values of its Kursliste.
-- While the year runs, today's wealth stands in for the year-end wealth,
-- because money set aside should not count on wealth growing.
-- There are no year-end values yet either, so it is valued with the latest
-- ESTV price of the year, and a market price where the ESTV has no price for
-- the year, like for most securities and coins.
computeWealthTax ::
  forall ann.
  (Ord ann) =>
  Currency ann ->
  Year ->
  Day ->
  SetAsideWealth ->
  SetAsidePrices ann ->
  Map AccountName (GenLocated ann (Account ann)) ->
  Vector (GenLocated ann (Transaction ann)) ->
  Validation (SetAsideError ann) (WealthTaxBasis, [SetAsideRow])
computeWealthTax chf year wealthTaxBasisDay wealth@SetAsideWealth {..} SetAsidePrices {..} accounts transactions = do
  let upToValuationDay :: Vector (GenLocated ann (Price ann)) -> Vector (GenLocated ann (Price ann))
      upToValuationDay =
        V.filter (\(Located _ Price {..}) -> Timestamp.toDay (locatedValue priceTimestamp) <= wealthTaxBasisDay)
  -- An ESTV price from an earlier year is a value for that year, not this
  -- one, and must not override a market price of this year either.
  let officialPrices :: Vector (GenLocated ann (Price ann))
      officialPrices =
        V.filter (\(Located _ Price {..}) -> dayInYear year (Timestamp.toDay (locatedValue priceTimestamp))) $
          upToValuationDay setAsidePricesOfficial
  let marketPrices :: Vector (GenLocated ann (Price ann))
      marketPrices = upToValuationDay setAsidePricesMarket
  let officialGraph = pricesToMemoisedPriceGraph officialPrices
  -- Prices are inserted in order, so official rates replace market ones for
  -- the same pair.
  let combinedGraph = pricesToMemoisedPriceGraph (marketPrices <> officialPrices)
  let hasPrice :: Vector (GenLocated ann (Price ann)) -> Currency ann -> Bool
      hasPrice ps currency =
        V.any (\(Located _ Price {..}) -> commodityCurrency (locatedValue priceCommodity) == currency) ps
  -- One currency, security or coin, in CHF.
  let valueHolding :: Currency ann -> Account.Account -> Validation (SetAsideError ann) Account.Account
      valueHolding currency amount =
        if currency == chf
          then pure amount
          else do
            graph <-
              if hasPrice officialPrices currency
                then pure officialGraph
                else
                  if hasPrice marketPrices currency
                    then pure combinedGraph
                    else validationFailure $ SetAsideErrorNoPrice (currencySymbol currency) wealthTaxBasisDay
            mapValidationFailure SetAsideErrorConvert $
              convertMultiAccountToAccount Nothing graph chf $
                MultiAccount.MultiAccount (M.singleton (CommodityCurrency currency) amount)

  let isWealth :: AccountName -> Bool
      isWealth an = case M.lookup an accounts of
        Just (Located _ Account {..}) -> case accountType of
          AccountTypeAssets -> True
          AccountTypeLiabilities -> True
          AccountTypeEquity -> False
          AccountTypeExpenses -> False
          AccountTypeIncome -> False
          AccountTypeOther -> False
        Nothing -> False
  balances <-
    maybe (validationFailure SetAsideErrorOverflow) pure $
      foldM
        ( \m (an, commodity, account) -> do
            let current = M.findWithDefault MultiAccount.zero an m
            new <- MultiAccount.addAccount current commodity account
            pure $ M.insert an new m
        )
        M.empty
        [ (an, commodity, locatedValue postingAccount)
        | Located _ Transaction {..} <- V.toList transactions,
          Timestamp.toDay (locatedValue transactionTimestamp) <= wealthTaxBasisDay,
          Located _ p@Posting {..} <- V.toList transactionPostings,
          postingReal,
          let an = locatedValue postingAccountName,
          isWealth an,
          let Located _ commodity = postingCommodity p
        ]
  taxableValues <-
    forM (M.toList (M.intersectionWith (,) balances accounts)) $ \(an, (balance, Located al Account {..})) -> do
      combined <- maybe (validationFailure SetAsideErrorOverflow) pure $ combineLots balance
      let holdings = M.toList (MultiAccount.unMultiAccount combined)
      if null holdings
        then pure Account.zero
        else case taxability wealth accountTags of
          NotTaxable -> pure Account.zero
          Untagged -> validationFailure $ SetAsideErrorUntaggedWealth al an
          TaggedBoth taxableLocation notTaxableLocation -> validationFailure $ SetAsideErrorTaggedBoth an taxableLocation notTaxableLocation
          Taxable -> do
            values <- mapM (uncurry valueHolding) holdings
            value <- requireSum values
            let ownedShare :: Scientific
                ownedShare = M.findWithDefault 1 an setAsideWealthOwnedShares
            requireRappen $ floor (fromInteger (Account.toMinimalQuantisations value) * toRational ownedShare)
  wealthTaxBasisTaxableWealth <- requireSum taxableValues
  (tariff, cantonalSteuerfuss) <-
    maybe (validationFailure $ SetAsideErrorNoCantonalRates year) pure $
      (,) <$> zurichMarriedWealthTariff year <*> zurichCantonalSteuerfuss year
  municipalSteuerfuss <-
    maybe (validationFailure $ SetAsideErrorNoMunicipalSteuerfuss setAsideWealthMunicipality year) pure $
      zurichMunicipalSteuerfuss setAsideWealthMunicipality year
  let simpleTax :: Rational
      simpleTax = simpleWealthTax tariff (fromInteger (Account.toMinimalQuantisations wealthTaxBasisTaxableWealth) / 100)
  wealthTaxBasisSimpleTax <- requireRappen $ round (simpleTax * 100)
  -- CHF times a Steuerfuss in percent is the tax in rappen.
  -- The bill shows the cantonal and the municipal part each on their own.
  cantonalTax <- requireRappen $ round (simpleTax * fromInteger cantonalSteuerfuss)
  municipalTax <- requireRappen $ round (simpleTax * fromInteger municipalSteuerfuss)
  pure
    ( WealthTaxBasis {..},
      [ SetAsideRowCantonalWealthTax cantonalSteuerfuss (negateAccount cantonalTax),
        SetAsideRowMunicipalWealthTax municipalSteuerfuss (negateAccount municipalTax)
      ]
    )

-- | The canton's Steuerfuss in percent, the same for everyone in the canton.
--
-- It is set for two years at a time, and is 95% for 2026 and 2027:
-- https://www.tagesanzeiger.ch/zuerich-kantonsrat-stimmt-ueber-steuerfuss-und-budget-2026-ab-583050897244
zurichCantonalSteuerfuss :: Year -> Maybe Integer
zurichCantonalSteuerfuss = \case
  2026 -> Just 95
  2027 -> Just 95
  _ -> Nothing

-- | A municipality's Steuerfuss in percent, on top of the canton's.
zurichMunicipalSteuerfuss :: Municipality -> Year -> Maybe Integer
zurichMunicipalSteuerfuss (Municipality number) year = case number of
  -- The city of Zürich.
  261 -> case year of
    -- https://frankenklar.ch/zh/261/
    2026 -> Just 119
    _ -> Nothing
  _ -> Nothing

-- | The wealth tax tariff for married couples, which also applies to a parent
-- who lives with their children (§ 47 Abs. 2 StG).
--
-- Each bracket is a width in CHF and a rate in per mille, and the last rate
-- applies to everything above the brackets.
-- The brackets move with inflation, so each year needs its own entry.
zurichMarriedWealthTariff :: Year -> Maybe ([(Integer, Rational)], Rational)
zurichMarriedWealthTariff = \case
  -- § 47 Abs. 2 StG as of 2026, in the ESTV summary of Zürich's tax law,
  -- "Stand: Februar 2026":
  -- https://www.estv2.admin.ch/stp/kb/zh-de.pdf
  2026 -> Just zurichMarriedWealthTariff2026
  -- The brackets only move at the start of each two-year Steuerfuss period
  -- (§ 48 Abs. 2 StG, in the same summary).
  2027 -> Just zurichMarriedWealthTariff2026
  _ -> Nothing

zurichMarriedWealthTariff2026 :: ([(Integer, Rational)], Rational)
zurichMarriedWealthTariff2026 =
  ( [ (161000, 0),
      (242000, 1 / 2),
      (402000, 1),
      (646000, 3 / 2),
      (967000, 2),
      (967000, 5 / 2)
    ],
    3
  )

-- | The simple state tax ("einfache Staatssteuer") in CHF, before
-- multiplying by the Steuerfuss in percent, on wealth in CHF.
--
-- The tax office rounds taxable wealth down to whole thousands, but this
-- does not: wealth that grows across a thousand by the end of the year would
-- make a rounded estimate too low, and money set aside must be enough.
simpleWealthTax :: ([(Integer, Rational)], Rational) -> Rational -> Rational
simpleWealthTax (brackets, topRate) wealth =
  let go :: Rational -> [(Integer, Rational)] -> Rational
      go rest = \case
        [] -> rest * topRate / 1000
        ((width, rate) : others) ->
          let part = min rest (fromInteger width)
           in part * rate / 1000 + go (rest - part) others
   in go (max 0 wealth) brackets
