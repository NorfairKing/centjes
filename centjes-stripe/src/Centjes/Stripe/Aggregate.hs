{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Turning a month's worth of Stripe's own financial reports into one aggregate.
--
-- Everything here is pure, because this is where a mistake becomes a wrong VAT
-- return.  Amounts are 'Account's carrying the sign the ledger posting will carry,
-- so that the emitter does no arithmetic of its own.
--
-- The one thing worth understanding before reading on: foreign revenue is a
-- /residual/, not something read off a report.  Stripe's tax report only has rows for
-- jurisdictions the account is registered in, so revenue from everywhere else is
-- structurally absent from it.  Taking foreign revenue to be the month's whole charge
-- revenue less what the tax report attributes to the home country means nothing can
-- go missing: revenue the tax report never mentions lands in foreign by construction,
-- and the balance check would catch it if it did not.
module Centjes.Stripe.Aggregate
  ( AggregateSettings (..),
    MonthReports (..),
    StripeMonth (..),
    RevenueAtRate (..),
    FeeLine (..),
    StripePayout (..),
    StripeError (..),
    swissVATRates,
    aggregateMonth,
    rowVATRate,
  )
where

import Centjes.Stripe.Currency
import Centjes.Stripe.Payout
import Centjes.Stripe.Report
import Centjes.Switzerland.Report.VAT.Types (allVATRates, vatRateRatio)
import Centjes.Validation
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Calendar.Month
import Error.Diagnose
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.Amount (Rounding (..))
import qualified Money.Amount as Amount
import Money.QuantisationFactor (QuantisationFactor)

-- | What the aggregator must be told rather than decide.
data AggregateSettings = AggregateSettings
  { -- | The two-letter country whose VAT this account owes, uppercase.
    --
    -- Revenue taxed here is domestic; everything else is foreign.  Tax collected for
    -- any /other/ country is money owed to that country rather than revenue, which
    -- this importer does not model, so it refuses instead.
    aggregateSettingHomeCountry :: !Text,
    -- | The VAT rates the home country's report can legitimately show.
    aggregateSettingVATRates :: !(NonEmpty Rational),
    -- | The currency the reports are read in, as Stripe spells it.
    --
    -- Checked against every row rather than assumed.  Three of the four reports are
    -- fetched with a currency filter, but the tax report takes none, so this is the only
    -- thing standing between a sale settled in euros and its amount being booked as
    -- francs.  That mistake is invisible to the reconciliation, which sees the month's
    -- total and not which side of the domestic split each figure went to.
    aggregateSettingCurrency :: !StripeCurrency,
    -- | The rate Stripe charges VAT on its own fees at.
    --
    -- Configured rather than read off the amounts, because a small fee cannot say which
    -- rate it was charged at: a fee of 18 rappen rounds to the same 1 rappen of tax at
    -- 8.1% as at 3.8%.  It is a property of where the account is registered rather than
    -- of the month, so there is nothing to infer month by month.
    --
    -- The amounts still get to object.  Whenever a month's fee is big enough to name a
    -- rate, that rate has to be this one, so a wrong setting fails in the first month
    -- that can tell rather than quietly claiming the wrong input tax forever.
    aggregateSettingFeesVATRate :: !Rational
  }

-- | The reports for one month, as fetched.
data MonthReports = MonthReports
  { monthReportsMonth :: !Month,
    -- | @tax.summarized_export.1@: one row per jurisdiction per rate.
    monthReportsTax :: !ReportTable,
    -- | @balance_change_from_activity.summary.2@: gross, fee and net per category.
    monthReportsActivity :: !ReportTable,
    -- | @all_fees.balance_transaction_created.summary.2@: fees and the tax on them.
    monthReportsFees :: !ReportTable,
    -- | @balance.summary.1@: the balance either side of the month.
    monthReportsBalance :: !ReportTable,
    monthReportsPayouts :: ![Payout]
  }

-- | One month of Stripe activity, ready to be written down.
data StripeMonth = StripeMonth
  { stripeMonthMonth :: !Month,
    -- | One entry per VAT rate the home country charged, sorted by rate.
    stripeMonthDomesticRevenues :: ![RevenueAtRate],
    -- | Revenue no home-country VAT was owed on, as a residual.
    stripeMonthForeignRevenue :: !(Maybe Money.Account),
    -- | How much of the month's charges no tax report row mentions at all, positive.
    --
    -- Not an error, and not something any guard can turn into one: such revenue lands in
    -- foreign by construction, and the month's total is right either way, so the
    -- reconciliation has nothing to say about it.  If any of it was in fact domestic, its
    -- VAT goes undeclared and nothing here would know.  So it is counted and reported,
    -- which is the most an importer can honestly do about it.
    --
    -- A subset of the foreign residual: the rest of the residual is revenue that rows for
    -- other countries do account for.
    stripeMonthRevenueWithNoTaxRow :: !Money.Account,
    -- | One entry per fee Stripe named, sorted by that name.
    stripeMonthFees :: ![FeeLine],
    stripeMonthPayouts :: ![StripePayout],
    -- | The balance Stripe reports at the start of the month.
    stripeMonthOpeningBalance :: !Money.Account,
    -- | The balance Stripe reports at the end of the month.
    stripeMonthClosingBalance :: !Money.Account
  }

-- | Revenue charged at one VAT rate, and anything refunded of it.
data RevenueAtRate = RevenueAtRate
  { -- | The rate as a fraction, so 0.081 rather than 8.1.
    revenueAtRateRate :: !Rational,
    -- | Revenue net of tax, negative because it is income.
    revenueAtRateNet :: !Money.Account,
    -- | Tax collected, negative because it is income.
    revenueAtRateTax :: !Money.Account,
    -- | Revenue refunded net of tax, positive because it reverses income.
    revenueAtRateRefundedNet :: !Money.Account,
    -- | Tax refunded, positive because it reverses income.
    revenueAtRateRefundedTax :: !Money.Account
  }
  deriving stock (Show, Eq)

-- | One line of Stripe's fees as Stripe named it, with the tax Stripe charged on it.
data FeeLine = FeeLine
  { feeLineDescription :: !Text,
    -- | Positive, because it is an expense.
    feeLineAmount :: !Money.Account,
    -- | The tax on that fee, positive, and zero for a fee that carried none.
    feeLineTax :: !Money.Account,
    -- | The rate that tax implies, and Nothing for a fee that carried none.
    --
    -- Worked out here rather than by the emitter, so that every rate this importer
    -- writes down was inferred in one place.
    feeLineRate :: !(Maybe Rational)
  }
  deriving stock (Show, Eq)

-- | One payout to the bank account.
data StripePayout = StripePayout
  { stripePayoutDay :: !Day,
    -- | Positive on the self-transfer account.
    stripePayoutAmount :: !Money.Account
  }

-- | The VAT rates a Swiss return can name.
--
-- Taken from @centjes-switzerland@ rather than written down again here.  A rate this
-- importer infers that the VAT report cannot name is a ledger that cannot be filed, so
-- the two sets have to be the same set rather than two lists that agree today.
swissVATRates :: NonEmpty Rational
swissVATRates = NE.map vatRateRatio allVATRates

data StripeError
  = StripeErrorReport !Month !ReportTypeId !ReportError
  | -- | Month, country, rate, tax collected.
    StripeErrorForeignTaxCollected !Month !Text !Rational !Money.Account
  | -- | Month, country, reported rate, taxable, tax collected.
    StripeErrorNoVATRate !Month !Text !Rational !Money.Account !Money.Account
  | -- | Month, country, taxable, nontaxable.
    StripeErrorHomeNontaxable !Month !Text !Money.Account !Money.Account
  | -- | Month, what the balance report says the month moved, what this importer books.
    StripeErrorDoesNotReconcile !Month !Money.Account !Money.Account
  | StripeErrorNoBalanceCategory !Month !Text
  | StripeErrorSum !Month
  | -- | A month whose refunds at some rate exceed its sales at that rate.
    StripeErrorRefundsExceedSales !Month !Rational
  | -- | Month, the configured fee VAT rate, the fee, the tax Stripe charged on it.
    StripeErrorFeeRateDisagrees !Month !Rational !Money.Account !Money.Account
  | -- | Month, which report, the currency the row stated.
    StripeErrorWrongCurrency !Month !ReportTypeId !Text
  deriving stock (Show)

instance ToReport StripeError where
  toReport =
    let stripeErr code message hints = Err (Just code) message [] (map Hint hints)
        inMonth month = unwords ["In", show month <> ":"]
     in \case
          StripeErrorReport month reportTypeId' reportError ->
            stripeErr
              "SE_REPORT"
              (unwords [inMonth month, "the report", show (unReportTypeId reportTypeId'), "could not be read."])
              [renderReportError reportError]
          StripeErrorForeignTaxCollected month country rate collected ->
            stripeErr
              "SE_FOREIGN_TAX_COLLECTED"
              ( unwords
                  [ inMonth month,
                    "Stripe collected",
                    show collected,
                    "of tax at",
                    showRate rate,
                    "for",
                    Text.unpack country <> ","
                  ]
              )
              [ "which is not the country this ledger owes VAT to.",
                "That money is owed to another tax authority rather than being revenue,",
                "and this importer does not know which account to hold it in.",
                "Book this month by hand."
              ]
          StripeErrorNoVATRate month country rate net collected ->
            stripeErr
              "SE_NO_VAT_RATE"
              ( unwords
                  [ inMonth month,
                    Text.unpack country,
                    "collected",
                    show collected,
                    "of tax on",
                    show net,
                    "at a reported rate of",
                    showRate rate <> ","
                  ]
              )
              [ "which is not a rate this ledger's VAT report can name.",
                "A rate centjes-switzerland cannot name cannot be declared."
              ]
          StripeErrorHomeNontaxable month country taxable nontaxable ->
            stripeErr
              "SE_HOME_NONTAXABLE"
              ( unwords
                  [ inMonth month,
                    Text.unpack country,
                    "reports",
                    show nontaxable,
                    "of non-taxable sales alongside",
                    show taxable,
                    "of taxable ones."
                  ]
              )
              [ "Revenue in the home country that carried no VAT is either exempt or zero-rated,",
                "and those are declared differently.  This importer will not guess which.",
                "Book this month by hand."
              ]
          StripeErrorDoesNotReconcile month moved booked ->
            stripeErr
              "SE_DOES_NOT_RECONCILE"
              ( unwords
                  [ inMonth month,
                    "Stripe's balance moved by",
                    show moved,
                    "but what this importer would book comes to",
                    show booked <> "."
                  ]
              )
              [ "Every figure here is one of Stripe's own aggregates, so a difference means the",
                "month holds something none of the four reports accounts for: a dispute, an",
                "adjustment or a transfer.",
                "Writing the month anyway would write a balance assertion that fails."
              ]
          StripeErrorNoBalanceCategory month category ->
            stripeErr
              "SE_NO_BALANCE_CATEGORY"
              (unwords [inMonth month, "the balance report has no", show category, "row."])
              ["Without it there is no balance to assert."]
          StripeErrorSum month ->
            stripeErr
              "SE_SUM"
              (unwords [inMonth month, "the amounts do not add up within the range of an account."])
              []
          StripeErrorRefundsExceedSales month rate ->
            stripeErr
              "SE_REFUNDS_EXCEED_SALES"
              (unwords [inMonth month, "more was refunded at", showRate rate, "than was sold at it."])
              [ "That makes the month's revenue at that rate positive, which the VAT report rejects.",
                "Book this month by hand."
              ]
          StripeErrorWrongCurrency month reportTypeId' stated ->
            stripeErr
              "SE_WRONG_CURRENCY"
              ( unwords
                  [ inMonth month,
                    show (unReportTypeId reportTypeId'),
                    "has a row in",
                    show stated <> ",",
                    "which is not the currency this ledger reads Stripe in."
                  ]
              )
              [ "Reading an amount in one currency as an amount in another is how a plausible wrong number gets booked.",
                "The tax report takes no currency filter, so a sale settled in another currency shows up here."
              ]
          StripeErrorFeeRateDisagrees month rate fee tax ->
            stripeErr
              "SE_FEE_RATE_DISAGREES"
              ( unwords
                  [ inMonth month,
                    "Stripe charged",
                    show tax,
                    "of tax on a fee of",
                    show fee <> ",",
                    "which",
                    showRate rate,
                    "does not account for."
                  ]
              )
              [ unwords ["The rate Stripe charges VAT on its fees at is configured, and is set to", showRate rate <> "."],
                "Either it has changed, or this month's fees were taxed at more than one rate.",
                "Check Stripe's tax invoice for the month and set fees-vat-rate to what it says."
              ]

showRate :: Rational -> String
showRate rate = show (fromRational (rate * 100) :: Double) <> "%"

-- | One row of the tax report, read.
data TaxRow = TaxRow
  { taxRowCountry :: !Text,
    taxRowReportedRate :: !Rational,
    taxRowTaxable :: !Money.Account,
    taxRowNontaxable :: !Money.Account,
    taxRowCollected :: !Money.Account,
    taxRowRefundedSales :: !Money.Account,
    taxRowRefundedTax :: !Money.Account
  }

-- | Aggregate one month's reports.
aggregateMonth ::
  AggregateSettings ->
  QuantisationFactor ->
  MonthReports ->
  Validation StripeError StripeMonth
aggregateMonth AggregateSettings {..} quantisationFactor MonthReports {..} = do
  let month = monthReportsMonth
  let amount = readAmount month quantisationFactor
  let text = readText month
  let rational = readRational month

  let inLedgerCurrency = requireRowCurrency month aggregateSettingCurrency

  taxRows <-
    traverse
      ( \row -> do
          inLedgerCurrency ReportKindTax (ReportColumn "transaction_currency") monthReportsTax row
          TaxRow
            . Text.toUpper
            <$> text ReportKindTax monthReportsTax (ReportColumn "country_code") row
            <*> rational ReportKindTax monthReportsTax (ReportColumn "tax_rate") row
            <*> amount ReportKindTax monthReportsTax (ReportColumn "total_taxable_sales") row
            <*> amount ReportKindTax monthReportsTax (ReportColumn "total_nontaxable_sales") row
            <*> amount ReportKindTax monthReportsTax (ReportColumn "total_tax_collected") row
            <*> amount ReportKindTax monthReportsTax (ReportColumn "total_sales_refunded") row
            <*> amount ReportKindTax monthReportsTax (ReportColumn "total_tax_refunded") row
      )
      (reportTableRows monthReportsTax)

  -- Tax collected anywhere but home is another authority's money, not revenue.
  traverse_
    ( \TaxRow {..} ->
        when (taxRowCountry /= aggregateSettingHomeCountry && taxRowCollected /= Account.zero) $
          validationFailure $
            StripeErrorForeignTaxCollected month taxRowCountry taxRowReportedRate taxRowCollected
    )
    taxRows

  -- Every home row that says anything at all, so that the guards below get to see it.
  -- Keeping only the rows with tax collected is how a home row whose tax was never
  -- computed used to reach the residual and be booked as foreign revenue, which is VAT
  -- that never gets declared and which the reconciliation cannot notice: the money is in
  -- the month either way, only on the wrong side of the split.
  let homeRows =
        [ taxRow
        | taxRow <- taxRows,
          taxRowCountry taxRow == aggregateSettingHomeCountry,
          any
            (/= Account.zero)
            [ taxRowTaxable taxRow,
              taxRowNontaxable taxRow,
              taxRowCollected taxRow,
              taxRowRefundedSales taxRow,
              taxRowRefundedTax taxRow
            ]
        ]

  stripeMonthDomesticRevenues <-
    sortOn revenueAtRateRate
      <$> traverse
        ( \TaxRow {..} ->
            -- A home-country row mixing taxable and non-taxable sales folds exempt or
            -- zero-rated revenue in with taxed revenue, and those are declared
            -- differently.
            if taxRowNontaxable /= Account.zero
              then validationFailure $ StripeErrorHomeNontaxable month taxRowCountry taxRowTaxable taxRowNontaxable
              else do
                rate <-
                  case rowVATRate aggregateSettingVATRates taxRowReportedRate taxRowTaxable taxRowCollected of
                    Nothing ->
                      validationFailure $
                        StripeErrorNoVATRate month taxRowCountry taxRowReportedRate taxRowTaxable taxRowCollected
                    Just rate -> pure rate
                net <- requireSubtract month taxRowTaxable taxRowRefundedSales
                if net < Account.zero
                  then validationFailure $ StripeErrorRefundsExceedSales month rate
                  else
                    pure
                      RevenueAtRate
                        { revenueAtRateRate = rate,
                          revenueAtRateNet = Account.negate taxRowTaxable,
                          revenueAtRateTax = Account.negate taxRowCollected,
                          revenueAtRateRefundedNet = taxRowRefundedSales,
                          revenueAtRateRefundedTax = taxRowRefundedTax
                        }
        )
        homeRows

  -- What the month charged in total, and the home country's share of it, tax
  -- included.  Foreign revenue is the difference, so nothing can fall out.
  chargeGross <- activityGross month aggregateSettingCurrency quantisationFactor monthReportsActivity "charge"
  homeGross <-
    requireSum month $
      concatMap (\TaxRow {..} -> [taxRowTaxable, taxRowCollected]) homeRows
  -- Against what was charged, not against what was charged less what was refunded: a
  -- refund is its own reporting category and never part of the charge total, so
  -- subtracting it here would turn every refund into foreign revenue.
  foreign_ <- requireSubtract month chargeGross homeGross
  let stripeMonthForeignRevenue =
        if foreign_ == Account.zero then Nothing else Just (Account.negate foreign_)

  -- What every row of the tax report accounts for, home and foreign alike, tax included.
  -- Whatever the month charged beyond that is revenue no row mentions.
  accountedGross <-
    requireSum month $
      concatMap
        (\TaxRow {..} -> [taxRowTaxable, taxRowNontaxable, taxRowCollected])
        taxRows
  stripeMonthRevenueWithNoTaxRow <- requireSubtract month chargeGross accountedGross

  stripeMonthFees <-
    sortOn feeLineDescription
      . filter ((/= Account.zero) . feeLineAmount)
      <$> traverse
        ( \row -> do
            inLedgerCurrency ReportKindFees (ReportColumn "currency") monthReportsFees row
            description <- text ReportKindFees monthReportsFees (ReportColumn "feature_name") row
            feeAmount <- amount ReportKindFees monthReportsFees (ReportColumn "amount") row
            feeTax <- amount ReportKindFees monthReportsFees (ReportColumn "tax") row
            -- A fee that carried no tax has no rate to write down, and must not get a
            -- VAT posting: the card processing fee is an exempt financial service.
            rate <-
              if feeTax == Account.zero
                then pure Nothing
                else
                  if rateFits aggregateSettingFeesVATRate feeAmount feeTax
                    then pure (Just aggregateSettingFeesVATRate)
                    else
                      validationFailure $
                        StripeErrorFeeRateDisagrees month aggregateSettingFeesVATRate feeAmount feeTax
            pure
              FeeLine
                { feeLineDescription = description,
                  feeLineAmount = feeAmount,
                  feeLineTax = feeTax,
                  feeLineRate = rate
                }
        )
        (reportTableRows monthReportsFees)

  stripeMonthPayouts <-
    traverse
      (\payout -> StripePayout (payoutDay payout) <$> accountOrFailure month (payoutAmount payout))
      ( sortOn payoutCreated $
          filter (payoutStatusMovedMoney . payoutStatus) monthReportsPayouts
      )

  stripeMonthOpeningBalance <- balanceCategory month aggregateSettingCurrency quantisationFactor monthReportsBalance "starting_balance"
  stripeMonthClosingBalance <- balanceCategory month aggregateSettingCurrency quantisationFactor monthReportsBalance "ending_balance"

  -- Everything above is one of Stripe's own aggregates, so what they say the month
  -- did has to equal what the ledger would say it did.
  moved <- requireSubtract month stripeMonthClosingBalance stripeMonthOpeningBalance
  booked <-
    requireSum month $
      concat
        [ map (Account.negate . revenueAtRateNet) stripeMonthDomesticRevenues,
          map (Account.negate . revenueAtRateTax) stripeMonthDomesticRevenues,
          map (Account.negate . revenueAtRateRefundedNet) stripeMonthDomesticRevenues,
          map (Account.negate . revenueAtRateRefundedTax) stripeMonthDomesticRevenues,
          maybeToList (Account.negate <$> stripeMonthForeignRevenue),
          map (Account.negate . feeLineAmount) stripeMonthFees,
          map (Account.negate . feeLineTax) stripeMonthFees,
          map (Account.negate . stripePayoutAmount) stripeMonthPayouts
        ]
  if moved /= booked
    then validationFailure $ StripeErrorDoesNotReconcile month moved booked
    else pure StripeMonth {stripeMonthMonth = month, ..}

-- | Which rate a home-country row was taxed at.
--
-- The report states it, so nothing is inferred: what the amounts get to do is object.  A
-- rate the ledger cannot name is refused too, because @centjes-switzerland@ accepts only
-- the three Swiss rates on a CHF posting, so writing any other would produce a ledger no
-- VAT report can read.
rowVATRate ::
  NonEmpty Rational ->
  -- | The rate the report states.
  Rational ->
  -- | Taxable sales.
  Money.Account ->
  -- | Tax collected.
  Money.Account ->
  Maybe Rational
rowVATRate nameable reported taxable collected =
  if reported `elem` NE.toList nameable && rateFits reported taxable collected
    then Just reported
    else Nothing

-- | Whether a rate is within a minor unit of accounting for this tax on this net.
--
-- A minor unit of slack rather than exact equality, because the rounding Stripe used is
-- not stated anywhere: nearest and truncating differ by a rappen and both are defensible.
-- Slack that wide is what makes a small fee unable to name its own rate, which is why the
-- fee rate is configured rather than inferred.
rateFits :: Rational -> Money.Account -> Money.Account -> Bool
rateFits rate net tax = case taxAt net rate of
  Nothing -> False
  Just atRate -> case Account.subtract atRate tax of
    Nothing -> False
    -- One minor unit of the currency, as an amount rather than as the number one.
    Just difference -> Account.abs difference <= Amount.fromMinimalQuantisations 1

-- | The tax a rate produces on an amount.
--
-- Left to 'Account.fraction' rather than done by hand on minor units, because rounding
-- a fraction of money is exactly the sort of arithmetic that has a right answer and
-- several plausible wrong ones.  Nearest, because that is how a tax is rounded.
taxAt :: Money.Account -> Rational -> Maybe Money.Account
taxAt net rate = fst (Account.fraction RoundNearest net rate)

activityGross ::
  Month ->
  StripeCurrency ->
  QuantisationFactor ->
  ReportTable ->
  Text ->
  Validation StripeError Money.Account
activityGross month currency quantisationFactor table category =
  case [ row
       | row <- reportTableRows table,
         columnOrEmpty (ReportColumn "reporting_category") row == category
       ] of
    -- No charges at all in a month is a real answer, not a missing row.
    [] -> pure Account.zero
    rows -> do
      traverse_ (requireRowCurrency month currency ReportKindActivity (ReportColumn "currency") table) rows
      requireSum month
        =<< traverse
          (readAmount month quantisationFactor ReportKindActivity table (ReportColumn "gross"))
          rows

balanceCategory ::
  Month ->
  StripeCurrency ->
  QuantisationFactor ->
  ReportTable ->
  Text ->
  Validation StripeError Money.Account
balanceCategory month currency quantisationFactor table category =
  case [ row
       | row <- reportTableRows table,
         columnOrEmpty (ReportColumn "category") row == category
       ] of
    [] -> validationFailure $ StripeErrorNoBalanceCategory month category
    (row : _) -> do
      requireRowCurrency month currency ReportKindBalance (ReportColumn "currency") table row
      readAmount month quantisationFactor ReportKindBalance table (ReportColumn "net_amount") row

-- | Refuse a row whose amounts mean something other than what they will be booked as.
--
-- A row that states no currency is let through, because the balance report leaves the
-- column empty on its payout lines.  The column itself still has to be there: it is asked
-- for by name when the report is created, so a column Stripe has renamed fails loudly
-- rather than turning this check off.
requireRowCurrency ::
  Month ->
  StripeCurrency ->
  ReportKind ->
  ReportColumn ->
  ReportTable ->
  ReportRow ->
  Validation StripeError ()
requireRowCurrency month currency kind column table row = do
  stated <- readText month kind table column row
  when (not (Text.null stated) && Text.toLower stated /= Text.toLower (unStripeCurrency currency)) $
    validationFailure $
      StripeErrorWrongCurrency month (reportKindTypeId kind) stated

readText ::
  Month ->
  ReportKind ->
  ReportTable ->
  ReportColumn ->
  ReportRow ->
  Validation StripeError Text
readText month kind table column row =
  liftReport month kind (requireColumn table column row)

readAmount ::
  Month ->
  QuantisationFactor ->
  ReportKind ->
  ReportTable ->
  ReportColumn ->
  ReportRow ->
  Validation StripeError Money.Account
readAmount month quantisationFactor kind table column row =
  liftReport month kind (requireAmount quantisationFactor table column row)

readRational ::
  Month ->
  ReportKind ->
  ReportTable ->
  ReportColumn ->
  ReportRow ->
  Validation StripeError Rational
readRational month kind table column row =
  liftReport month kind (requireRational table column row)

liftReport :: Month -> ReportKind -> Either ReportError a -> Validation StripeError a
liftReport month kind = \case
  Left reportError -> validationFailure $ StripeErrorReport month (reportKindTypeId kind) reportError
  Right a -> pure a

requireSum :: Month -> [Money.Account] -> Validation StripeError Money.Account
requireSum month accounts = case Account.sum accounts of
  Nothing -> validationFailure $ StripeErrorSum month
  Just total -> pure total

requireSubtract :: Month -> Money.Account -> Money.Account -> Validation StripeError Money.Account
requireSubtract month a b = case Account.subtract a b of
  Nothing -> validationFailure $ StripeErrorSum month
  Just difference -> pure difference

accountOrFailure :: Month -> Integer -> Validation StripeError Money.Account
accountOrFailure month i = case Account.fromMinimalQuantisations i of
  Nothing -> validationFailure $ StripeErrorSum month
  Just a -> pure a
