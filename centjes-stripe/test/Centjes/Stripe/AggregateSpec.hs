{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes.Stripe.AggregateSpec (spec) where

import Centjes.Stripe.Aggregate
import Centjes.Stripe.Currency
import Centjes.Stripe.Payout
import Centjes.Stripe.Report
import Centjes.Stripe.Timestamp
import Centjes.Validation
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar.Month
import qualified Money.Account as Account
import Money.QuantisationFactor (QuantisationFactor (..))
import Test.Syd

spec :: Spec
spec = do
  describe "rowVATRate" $ do
    it "takes the rate the report states" $
      rowVATRate swissVATRates 0.081 (account 100_000) (account 8_100) `shouldBe` Just 0.081

    it "takes a reduced rate the report states" $
      rowVATRate swissVATRates 0.026 (account 100_000) (account 2_600) `shouldBe` Just 0.026

    -- Stripe rounds each charge's tax and then adds them up, so a month's total can sit a
    -- rappen off the rate applied to the month's total.
    it "allows the amounts to be a minor unit off the stated rate" $
      rowVATRate swissVATRates 0.081 (account 100_000) (account 8_101) `shouldBe` Just 0.081

    -- The check that matters: the report saying one rate while the money says another means
    -- one of the two is not what it appears to be, and guessing which would be a guess
    -- about someone's VAT.
    it "refuses a stated rate the amounts do not bear out" $
      rowVATRate swissVATRates 0.081 (account 100_000) (account 2_600) `shouldBe` Nothing

    -- centjes-switzerland accepts only the three Swiss rates on a CHF posting, so any other
    -- would make a ledger no VAT report can read.
    it "refuses a rate the ledger cannot name" $
      rowVATRate swissVATRates 0.21 (account 100_000) (account 21_000) `shouldBe` Nothing

  describe "aggregateMonth" $ do
    it "aggregates an ordinary month" $ do
      reports <- monthReports (YearMonth 2026 7) []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> do
          stripeMonthDomesticRevenues month
            `shouldBe` [ RevenueAtRate
                           { revenueAtRateRate = 0.081,
                             revenueAtRateNet = account (-100_000),
                             revenueAtRateTax = account (-8_100),
                             revenueAtRateRefundedNet = Account.zero,
                             revenueAtRateRefundedTax = Account.zero
                           }
                       ]
          -- The Belgian row: nothing collected, so it is foreign, and it arrives as the
          -- residual rather than being read off the tax report.
          stripeMonthForeignRevenue month `shouldBe` Just (account (-50_000))
          stripeMonthFees month
            `shouldBe` [ FeeLine "Card payments - Stripe fee" (account 2_000) Account.zero Nothing,
                         FeeLine "Stripe Tax - integration fee" (account 1_000) (account 81) (Just 0.081)
                       ]
          stripeMonthOpeningBalance month `shouldBe` account 10_000
          stripeMonthClosingBalance month `shouldBe` account 165_019

    -- The bug this exists for: taking foreign revenue to be charges less the home
    -- country's net of refunds turns every refunded franc into foreign revenue.
    it "does not turn a refund into foreign revenue" $ do
      reports <- monthReports (YearMonth 2026 8) []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> do
          stripeMonthForeignRevenue month `shouldBe` Nothing
          map revenueAtRateRefundedNet (stripeMonthDomesticRevenues month) `shouldBe` [account 10_000]
          map revenueAtRateRefundedTax (stripeMonthDomesticRevenues month) `shouldBe` [account 810]

    it "reads the fee that carried no tax as carrying no rate" $ do
      reports <- monthReports (YearMonth 2026 7) []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month ->
          map feeLineRate (stripeMonthFees month) `shouldBe` [Nothing, Just 0.081]

    -- Tax collected for somewhere else is that authority's money, not revenue, and this
    -- importer has nowhere to hold it.
    it "refuses tax collected for another country" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("tax.csv", "\"country_code\",\"jurisdiction_level\",\"jurisdiction_name\",\"tax_rate\",\"transaction_currency\",\"total_sales\",\"total_tax_collected\",\"total_taxable_sales\",\"total_nontaxable_sales\",\"total_sales_refunded\",\"total_tax_refunded\"\n\"DE\",\"country\",\"GERMANY\",\"0.19\",\"chf\",\"100.00\",\"19.00\",\"100.00\",\"0.00\",\"0.00\",\"0.00\"\n")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isForeignTaxCollected
        Success _ -> expectationFailure "Expected foreign tax collected to be refused"

    -- Exempt and zero-rated home revenue are declared differently, and this importer
    -- cannot tell which it is looking at.
    it "refuses non-taxable revenue in the home country" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("tax.csv", "\"country_code\",\"jurisdiction_level\",\"jurisdiction_name\",\"tax_rate\",\"transaction_currency\",\"total_sales\",\"total_tax_collected\",\"total_taxable_sales\",\"total_nontaxable_sales\",\"total_sales_refunded\",\"total_tax_refunded\"\n\"CH\",\"country\",\"SWITZERLAND\",\"0.081\",\"chf\",\"200.00\",\"8.10\",\"100.00\",\"100.00\",\"0.00\",\"0.00\"\n")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isHomeNontaxable
        Success _ -> expectationFailure "Expected non-taxable home revenue to be refused"

    -- The dangerous shape, because the reconciliation cannot catch it: booking the wrong
    -- side of the domestic split leaves the month's total untouched.  A home row whose tax
    -- was never computed used to be dropped before either guard saw it, and its revenue
    -- then arrived as foreign by residual, which is VAT never declared.
    it "refuses home revenue whose tax was never computed" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("tax.csv", taxCsv [taxRow "CH" "0.081" "100.00" "0.00" "0.00" "0.00"]),
            ("balance.csv", balanceCsv "1650.19")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isNoVATRate
        Success month ->
          expectationFailure $
            unwords
              [ "Expected untaxed home revenue to be refused, but it was booked as",
                show (stripeMonthForeignRevenue month)
              ]

    it "refuses non-taxable home revenue even when no tax was collected" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("tax.csv", taxCsv [taxRow "CH" "0.081" "0.00" "100.00" "0.00" "0.00"]),
            ("balance.csv", balanceCsv "1650.19")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isHomeNontaxable
        Success month ->
          expectationFailure $
            unwords
              [ "Expected non-taxable home revenue to be refused, but it was booked as",
                show (stripeMonthForeignRevenue month)
              ]

    it "ignores a home row that is all zeroes" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("tax.csv", taxCsv [taxRow "CH" "0.081" "0.00" "0.00" "0.00" "0.00"]),
            ("balance.csv", balanceCsv "1650.19")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> stripeMonthDomesticRevenues month `shouldBe` []

    -- The tax report takes no currency filter, so a row in another currency arrives with
    -- an amount that means something else.  Reading it as the ledger's currency moves the
    -- domestic split without moving the month's total, which is exactly what the
    -- reconciliation cannot see.
    it "refuses a tax row in another currency" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ( "tax.csv",
              LB.concat
                [ taxCsvHeader,
                  "\"CH\",\"country\",\"SWITZERLAND\",\"0.081\",\"eur\",\"108.10\",\"8.10\",\"100.00\",\"0.00\",\"0.00\",\"0.00\"\n"
                ]
            )
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isWrongCurrency
        Success _ -> expectationFailure "Expected a row in another currency to be refused"

    it "refuses a fee row in another currency" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ( "fees.csv",
              LB.concat
                [ "\"suite\",\"product\",\"feature_name\",\"amount\",\"tax\",\"currency\"\n",
                  "\"Payments\",\"Payment Processing\",\"Card payments - Stripe fee\",\"20.00\",\"0.00\",\"eur\"\n"
                ]
            ),
            ("balance.csv", balanceCsv "1661.00")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isWrongCurrency
        Success _ -> expectationFailure "Expected a fee in another currency to be refused"

    -- The balance report leaves the currency off its payout lines, so an absent currency
    -- has to stay acceptable or every month would be refused.
    it "accepts a row that states no currency at all" $ do
      reports <- monthReports (YearMonth 2026 7) []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> stripeMonthClosingBalance month `shouldBe` account 165_019

    -- Nothing can catch revenue that no tax row mentions: it lands in foreign by
    -- construction and the money is in the month either way.  What the importer can do is
    -- say how much of the month is in that position, which is the figure worth looking at
    -- before filing.
    it "counts the revenue no tax row accounts for" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ( "tax.csv",
              taxCsv
                [ taxRow "BE" "0.21" "0.00" "400.00" "0.00" "0.00",
                  taxRow "CH" "0.081" "1000.00" "0.00" "81.00" "0.00"
                ]
            )
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> do
          -- Charges of 1581.00 against rows accounting for 1481.00 of it.
          stripeMonthRevenueWithNoTaxRow month `shouldBe` account 10_000
          -- And it is not the same thing as foreign revenue, which is the whole residual.
          stripeMonthForeignRevenue month `shouldBe` Just (account (-50_000))

    it "counts nothing unaccounted when the rows add up to the charges" $ do
      reports <- monthReports (YearMonth 2026 7) []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> stripeMonthRevenueWithNoTaxRow month `shouldBe` Account.zero

    -- Every figure is one of Stripe's own aggregates, so if they disagree the month
    -- holds something none of the four reports accounts for.
    it "refuses a month whose reports do not add up" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("balance.csv", "\"category\",\"description\",\"net_amount\",\"currency\"\n\"starting_balance\",\"Starting balance\",\"100.00\",\"chf\"\n\"ending_balance\",\"Ending balance\",\"999.99\",\"chf\"\n")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isDoesNotReconcile
        Success _ -> expectationFailure "Expected a month that does not reconcile to be refused"

    it "refuses a balance report with no ending balance" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("balance.csv", "\"category\",\"description\",\"net_amount\",\"currency\"\n\"starting_balance\",\"Starting balance\",\"100.00\",\"chf\"\n")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isNoBalanceCategory
        Success _ -> expectationFailure "Expected a missing ending balance to be refused"

    -- A fee small enough that its tax cannot name a rate: 18 rappen of fee rounds to the
    -- same 1 rappen of tax at 8.1% as at 3.8%.  Stripe's tax on its own fees is a
    -- property of the account's jurisdiction rather than of the month, so it is
    -- configured, and the amounts only get to object.
    it "takes the configured rate for a fee too small to imply one" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("fees.csv", feesCsv "0.18" "0.01"),
            ("balance.csv", balanceCsv "1680.81")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> map feeLineRate (stripeMonthFees month) `shouldBe` [Just 0.081]

    -- The other side of configuring it: a month whose amounts can name a rate must agree
    -- with the configured one, or a wrong setting would quietly claim the wrong input tax
    -- every month.
    it "refuses a fee whose tax the configured rate cannot account for" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("fees.csv", feesCsv "10.00" "0.26"),
            ("balance.csv", balanceCsv "1670.74")
          ]
          []
      case aggregateMonth settings chf reports of
        Failure errs -> foldr (:) [] errs `shouldSatisfy` any isFeeRateDisagrees
        Success _ -> expectationFailure "Expected a fee at the wrong rate to be refused"

    it "counts each payout it was given" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [ ("balance.csv", "\"category\",\"description\",\"net_amount\",\"currency\"\n\"starting_balance\",\"Starting balance\",\"100.00\",\"chf\"\n\"ending_balance\",\"Ending balance\",\"1450.19\",\"chf\"\n")
          ]
          [ payoutOf "po_1" 1_783_000_000 10_000 PayoutStatusPaid,
            payoutOf "po_2" 1_783_100_000 10_000 PayoutStatusPaid
          ]
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> map stripePayoutAmount (stripeMonthPayouts month) `shouldBe` [account 10_000, account 10_000]

    -- A payout that failed or was canceled has its money returned to the balance, so
    -- booking it would move money in the ledger that never left Stripe.
    it "does not book a payout that never happened" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [("balance.csv", balanceCsv "1650.19")]
          [ payoutOf "po_1" 1_783_000_000 10_000 PayoutStatusFailed,
            payoutOf "po_2" 1_783_100_000 10_000 PayoutStatusCanceled
          ]
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> map stripePayoutAmount (stripeMonthPayouts month) `shouldBe` []

    -- Stripe takes the money out of the balance when the payout is created, so one still
    -- on its way has already left.
    it "books a payout that is still on its way" $ do
      reports <-
        monthReportsWith
          (YearMonth 2026 7)
          [("balance.csv", "\"category\",\"description\",\"net_amount\",\"currency\"\n\"starting_balance\",\"Starting balance\",\"100.00\",\"chf\"\n\"ending_balance\",\"Ending balance\",\"1550.19\",\"chf\"\n")]
          [payoutOf "po_1" 1_783_000_000 10_000 PayoutStatusInTransit]
      case aggregateMonth settings chf reports of
        Failure errs -> expectationFailure $ show errs
        Success month -> map stripePayoutAmount (stripeMonthPayouts month) `shouldBe` [account 10_000]

taxCsvHeader :: LB.ByteString
taxCsvHeader =
  LB.concat
    [ "\"country_code\",\"jurisdiction_level\",\"jurisdiction_name\",\"tax_rate\",",
      "\"transaction_currency\",\"total_sales\",\"total_tax_collected\",\"total_taxable_sales\",",
      "\"total_nontaxable_sales\",\"total_sales_refunded\",\"total_tax_refunded\"\n"
    ]

taxCsv :: [LB.ByteString] -> LB.ByteString
taxCsv rows = LB.concat (taxCsvHeader : rows)

-- | One tax report row: country, rate, taxable, non-taxable, collected, refunded sales.
taxRow ::
  LB.ByteString ->
  LB.ByteString ->
  LB.ByteString ->
  LB.ByteString ->
  LB.ByteString ->
  LB.ByteString ->
  LB.ByteString
taxRow country rate taxable nontaxable collected refunded =
  LB.concat
    [ "\"",
      country,
      "\",\"country\",\"SOMEWHERE\",\"",
      rate,
      "\",\"chf\",\"0.00\",\"",
      collected,
      "\",\"",
      taxable,
      "\",\"",
      nontaxable,
      "\",\"",
      refunded,
      "\",\"0.00\"\n"
    ]

-- | A fee report with one taxed fee in it.
feesCsv :: LB.ByteString -> LB.ByteString -> LB.ByteString
feesCsv amount tax =
  LB.concat
    [ "\"suite\",\"product\",\"feature_name\",\"amount\",\"tax\",\"currency\"\n",
      "\"Revenue management\",\"Tax\",\"Stripe Tax - integration fee\",\"",
      amount,
      "\",\"",
      tax,
      "\",\"chf\"\n"
    ]

-- | A balance report closing on a given figure, opening on the one the fixtures use.
balanceCsv :: LB.ByteString -> LB.ByteString
balanceCsv ending =
  LB.concat
    [ "\"category\",\"description\",\"net_amount\",\"currency\"\n",
      "\"starting_balance\",\"Starting balance\",\"100.00\",\"chf\"\n",
      "\"ending_balance\",\"Ending balance\",\"",
      ending,
      "\",\"chf\"\n"
    ]

isForeignTaxCollected, isHomeNontaxable, isDoesNotReconcile, isNoBalanceCategory, isFeeRateDisagrees, isNoVATRate, isWrongCurrency :: StripeError -> Bool
isForeignTaxCollected = \case StripeErrorForeignTaxCollected {} -> True; _ -> False
isHomeNontaxable = \case StripeErrorHomeNontaxable {} -> True; _ -> False
isDoesNotReconcile = \case StripeErrorDoesNotReconcile {} -> True; _ -> False
isNoBalanceCategory = \case StripeErrorNoBalanceCategory {} -> True; _ -> False
isFeeRateDisagrees = \case StripeErrorFeeRateDisagrees {} -> True; _ -> False
isNoVATRate = \case StripeErrorNoVATRate {} -> True; _ -> False
isWrongCurrency = \case StripeErrorWrongCurrency {} -> True; _ -> False

settings :: AggregateSettings
settings =
  AggregateSettings
    { aggregateSettingHomeCountry = "CH",
      aggregateSettingVATRates = swissVATRates,
      aggregateSettingCurrency = StripeCurrency "chf",
      aggregateSettingFeesVATRate = 0.081
    }

chf :: QuantisationFactor
chf = QuantisationFactor 100

account :: Integer -> Account.Account
account i = case Account.fromMinimalQuantisations i of
  Nothing -> error $ unwords ["Not a valid account in this test:", show i]
  Just a -> a

-- | The fixture reports for a month.
--
-- The CSVs under @test_resources\/reports@ have the shape of real responses from a live
-- account and invented amounts, because this repository is public and the real ones are
-- the account holder's revenue.  The shape is reproduced exactly, and these parts of it
-- are load-bearing rather than incidental:
--
-- * column order as Stripe emits it, and columns this importer does not ask for, so that
--   reading by name is what is being tested,
-- * two decimal places in the tax, activity and balance reports but /eighteen/ in the fee
--   report, because that is what Stripe really sends,
-- * an empty @currency@ on the balance report's payout rows, which is why a row stating no
--   currency has to stay acceptable while every row that states one has to state the
--   ledger's,
-- * a @tax_rate@ of @0.21@ on a Belgian row that collected nothing, because Stripe states
--   the jurisdiction's own rate whether or not it applied.  Only home-country rows take
--   their rate from that column.
--
-- The amounts reconcile exactly, which is the point: 'aggregateMonth' refuses a month whose
-- reports do not add up, so a fixture that did not add up would test only the refusal.
monthReports :: Month -> [Payout] -> IO MonthReports
monthReports month = monthReportsWith month []

-- | The fixture reports for a month, with some of them replaced.
monthReportsWith :: Month -> [(FilePath, LB.ByteString)] -> [Payout] -> IO MonthReports
monthReportsWith month overrides payouts = do
  let table name = case lookup name overrides of
        Just contents -> parse name contents
        Nothing -> do
          contents <- LB.readFile ("test_resources/reports/" <> show month <> "/" <> name)
          parse name contents
      parse name contents = case parseReportTable contents of
        Left e -> expectationFailure (unwords [name <> ":", renderReportError e]) >> error "unreachable"
        Right t -> pure t
  MonthReports month
    <$> table "tax.csv"
    <*> table "activity.csv"
    <*> table "fees.csv"
    <*> table "balance.csv"
    <*> pure payouts

payoutOf :: Text -> Int64 -> Integer -> PayoutStatus -> Payout
payoutOf identifier created minorUnits status =
  Payout
    { payoutId = PayoutId identifier,
      payoutCreated = StripeTimestamp created,
      payoutCurrency = StripeCurrency "chf",
      payoutAmount = minorUnits,
      payoutStatus = status
    }
