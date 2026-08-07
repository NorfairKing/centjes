{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Centjes.Stripe.DeclarationsSpec (spec) where

import Centjes.Format (formatModule)
import Centjes.Location
import Centjes.Module
import Centjes.Parse (parseModule)
import Centjes.Parse.TestUtils (shouldParse)
import Centjes.Stripe.Aggregate
import Centjes.Stripe.Declarations
import Centjes.Validation
import Data.List (inits)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.Month
import qualified Money.Account as Account
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec = do
  describe "stripeTransactions" $ do
    it "writes an ordinary month" $
      goldenTextFile "test_resources/declarations/ordinary.cent" $
        pure $
          render (emit [july])

    it "writes a month with refunds, and the opening balance before it" $
      goldenTextFile "test_resources/declarations/refunds.cent" $
        pure $
          render (emit [july, august])

    it "writes nothing at all for a month with nothing in it" $
      map descriptionOf (emit [emptyMonth]) `shouldBe` []

    it "names the transactions of a month in the order they must be written" $
      map descriptionOf (emit [july])
        `shouldBe` [ Just "Stripe opening balance",
                     Just "Stripe payout 2026-07-15",
                     Just "Stripe sales 2026-07",
                     Just "Stripe fees 2026-07"
                   ]

    -- This is the invariant produceVATReport depends on and centjes check does not
    -- enforce: it pairs each posting with its successor, so a domestic income posting
    -- whose successor is not the VAT posting is not an error but a silently wrong return.
    it "follows every domestic income posting immediately with its VAT posting" $
      case transactionNamed "Stripe sales 2026-07" (emit [july]) of
        Nothing -> expectationFailure "Expected a sales transaction"
        Just sales -> do
          accountNamesOf sales
            `shouldBe` ["income:domestic", "income:VAT", "income:foreign", "assets:stripe"]
          ratiosOf sales `shouldBe` [False, True, False, False]

    -- Two rates in one month are two adjacent pairs, never one pair with a stray posting
    -- between them.  produceVATReport reads the rate off each income posting's successor,
    -- so an interleaved order would attribute the second rate's revenue to the first rate's
    -- VAT posting and silently file the wrong numbers.
    it "keeps each rate's pair together when a month has two rates" $
      case transactionNamed "Stripe sales 2026-07" (emit [july {stripeMonthDomesticRevenues = twoRates}]) of
        Nothing -> expectationFailure "Expected a sales transaction"
        Just sales -> do
          accountNamesOf sales
            `shouldBe` [ "income:domestic",
                         "income:VAT",
                         "income:domestic",
                         "income:VAT",
                         "income:foreign",
                         "assets:stripe"
                       ]
          ratiosOf sales `shouldBe` [False, True, False, True, False, False]
          ratioPercentagesOf sales `shouldBe` [Nothing, Just "2.6", Nothing, Just "8.1", Nothing, Nothing]

    it "keeps each rate's pair together when a month refunded at two rates" $
      case transactionNamed "Stripe refunds 2026-07" (emit [july {stripeMonthDomesticRevenues = twoRates}]) of
        Nothing -> expectationFailure "Expected a refunds transaction"
        Just refunds -> do
          accountNamesOf refunds
            `shouldBe` [ "expenses:refunds",
                         "expenses:VAT",
                         "expenses:refunds",
                         "expenses:VAT",
                         "assets:stripe"
                       ]
          ratioPercentagesOf refunds `shouldBe` [Nothing, Just "2.6", Nothing, Just "8.1", Nothing]

    -- The fee that carried VAT gets a VAT posting; the exempt card fee must not, and its
    -- posting being followed by something else is how the VAT report skips it.
    it "follows only the fee that carried tax with a VAT posting" $
      case transactionNamed "Stripe fees 2026-07" (emit [july]) of
        Nothing -> expectationFailure "Expected a fees transaction"
        Just fees -> do
          accountNamesOf fees
            `shouldBe` [ "expenses:banking:stripe",
                         "expenses:banking:stripe",
                         "expenses:VAT",
                         "assets:stripe"
                       ]
          ratiosOf fees `shouldBe` [False, False, True, False]

    it "tags a fee transaction that reclaimed VAT as deductible" $
      case transactionNamed "Stripe fees 2026-07" (emit [july]) of
        Nothing -> expectationFailure "Expected a fees transaction"
        Just fees -> tagsOf fees `shouldBe` ["deductible"]

    -- A transaction tagged deductible must have at least one expense posting followed by
    -- a VAT posting, or centjes-switzerland fails with VATErrorDeductibleNoExpenses.
    it "tags a fee transaction that reclaimed nothing as not VAT deductible" $
      case transactionNamed "Stripe fees 2026-07" (emit [july {stripeMonthFees = [exemptFeeOnly]}]) of
        Nothing -> expectationFailure "Expected a fees transaction"
        Just fees -> tagsOf fees `shouldBe` ["tax-deductible", "not-vat-deductible"]

    -- Refunds are an expense rather than negative revenue, because the VAT report rejects
    -- a positive posting to an income account outright.
    it "writes a refund as an expense followed by the VAT on it" $
      case transactionNamed "Stripe refunds 2026-08" (emit [august]) of
        Nothing -> expectationFailure "Expected a refunds transaction"
        Just refunds -> do
          accountNamesOf refunds `shouldBe` ["expenses:refunds", "expenses:VAT", "assets:stripe"]
          ratiosOf refunds `shouldBe` [False, True, False]
          tagsOf refunds `shouldBe` ["deductible", "duplicate-attachment"]

    -- The evidence for a month is the report it was computed from, saved by the importer,
    -- so a transaction refers to a file that is on disk by the time it is written.
    it "attaches the reports each transaction was computed from" $
      map attachmentsOf (emit [july])
        `shouldBe` [ [],
                     [],
                     [ "documents/stripe/reports/2026-07-31_stripe-tax-summary-2026-07.csv",
                       "documents/stripe/reports/2026-07-31_stripe-activity-summary-2026-07.csv"
                     ],
                     [ "documents/stripe/2026-07-31_tax-invoice-2026-07.pdf",
                       "documents/stripe/reports/2026-07-31_stripe-fee-summary-2026-07.csv"
                     ]
                   ]

    -- One report says both what was sold and what was given back, so in a month with
    -- refunds that file is attached twice on purpose.  centjes check refuses that without
    -- the tag on both transactions, which is the rule doing its job.
    it "says when it attaches the same report twice" $ do
      let sales = maybeToList (transactionNamed "Stripe sales 2026-08" (emit [august]))
      let refunds = maybeToList (transactionNamed "Stripe refunds 2026-08" (emit [august]))
      let reportsDirectory = "documents/stripe/reports/"
      let taxSummary = reportsDirectory <> "2026-08-31_stripe-tax-summary-2026-08.csv"
      map attachmentsOf sales
        `shouldBe` [[taxSummary, reportsDirectory <> "2026-08-31_stripe-activity-summary-2026-08.csv"]]
      map attachmentsOf refunds `shouldBe` [[taxSummary]]
      map (elem "duplicate-attachment" . tagsOf) (sales ++ refunds) `shouldBe` [True, True]

    -- The other side of that: claiming a duplicate that is not there would let a real
    -- copy-paste mistake through in every month without refunds.
    it "does not claim a duplicate attachment in a month without refunds" $
      case transactionNamed "Stripe sales 2026-07" (emit [july]) of
        Nothing -> expectationFailure "Expected a sales transaction"
        Just sales -> tagsOf sales `shouldBe` []

    -- Nor in a month that refunded without selling, where the report is attached once.
    it "does not claim a duplicate attachment in a month with refunds and no sales" $ do
      let emitted = emit [refundsOnly]
      map descriptionOf emitted `shouldBe` [Just "Stripe refunds 2026-08"]
      map tagsOf emitted `shouldBe` [["deductible"]]

    it "asserts the balance exactly once per month, on the last transaction" $
      map (length . assertionsOf) (emit [july, august]) `shouldBe` [0, 0, 0, 1, 0, 1]

    it "emits transactions whose postings sum to zero" $
      map (Account.sum . map amountOf . postingsOf) (emit [july, august])
        `shouldBe` map (const (Just Account.zero)) (emit [july, august])

    -- The assertion is the whole reason to import at all: it is what makes a wrong month
    -- fail centjes check rather than sit in the ledger looking plausible.  So what the
    -- transactions add up to has to be what the assertion claims.
    it "asserts a balance that its own postings add up to" $
      assertedAgainstRunningTotal (emit [july, august])
        `shouldBe` [ (Just (account 145_019), Just (account 145_019)),
                     (Just (account 242_309), Just (account 242_309))
                   ]

    it "puts everything it emits back through the parser unchanged" $ do
      here <- getCurrentDir
      let rendered = render (emit [july, august])
      parsed <- shouldParse parseModule here [relfile|stripe.cent|] rendered
      render' (map (noLoc . stripDeclarationAnnotation . locatedValue) (moduleDeclarations parsed))
        `shouldBe` rendered

emit :: [StripeMonth] -> [Transaction ()]
emit months =
  case stripeTransactions testSettings (QuantisationFactor 100) (CurrencySymbol "CHF") months of
    Failure errs -> error $ unwords ["Could not emit transactions in this test:", show errs]
    Success transactions -> transactions

render :: [Transaction ()] -> Text
render = render' . map (noLoc . DeclarationTransaction . noLoc)

render' :: [GenLocated () (Declaration ())] -> Text
render' declarations = formatModule Module {moduleImports = [], moduleDeclarations = declarations}

descriptionOf :: Transaction () -> Maybe Text
descriptionOf t = unDescription . locatedValue . commentedValue <$> transactionDescription t

transactionNamed :: Text -> [Transaction ()] -> Maybe (Transaction ())
transactionNamed name transactions = case filter ((== Just name) . descriptionOf) transactions of
  [] -> Nothing
  (t : _) -> Just t

accountNamesOf :: Transaction () -> [AccountName]
accountNamesOf t =
  [locatedValue (postingAccountName p) | Commented (Located _ p) _ <- transactionPostings t]

ratiosOf :: Transaction () -> [Bool]
ratiosOf t = [isJust (postingRatio p) | Commented (Located _ p) _ <- transactionPostings t]

tagsOf :: Transaction () -> [Tag]
tagsOf t =
  [ locatedValue lt
  | Commented (Located _ (TransactionTag (Located _ (ExtraTag lt)))) _ <- transactionExtras t
  ]

assertionsOf :: Transaction () -> [ExtraAssertion ()]
assertionsOf t =
  [locatedValue lea | Commented (Located _ (TransactionAssertion lea)) _ <- transactionExtras t]

attachmentsOf :: Transaction () -> [FilePath]
attachmentsOf t =
  [ fromRelFile rf
  | Commented (Located _ (TransactionAttachment (Located _ (ExtraAttachment (Located _ (Attachment (Located _ rf))))))) _ <-
      transactionExtras t
  ]

postingsOf :: Transaction () -> [Posting ()]
postingsOf t = [p | Commented (Located _ p) _ <- transactionPostings t]

amountOf :: Posting () -> Account.Account
amountOf p = literalAsAccount (locatedValue (postingAccount p))

literalAsAccount :: DecimalLiteral -> Account.Account
literalAsAccount literal = case Account.fromDecimalLiteral (QuantisationFactor 100) literal of
  Nothing -> error $ unwords ["Not a centime-sized amount in this test:", show literal]
  Just a -> a

-- | For every balance assertion, what it claims paired with what the transactions up to
-- and including it actually add up to.
assertedAgainstRunningTotal ::
  [Transaction ()] ->
  [(Maybe Account.Account, Maybe Account.Account)]
assertedAgainstRunningTotal transactions =
  [ (claimed, runningTotal)
  | (t, earlier) <- zip transactions (inits transactions),
    ExtraAssertion (Located _ (AssertionEquals _ (Located _ name) (Located _ literal) _)) <- assertionsOf t,
    name == declarationSettingAssetsAccountName testSettings,
    let claimed = Just (literalAsAccount literal),
    let runningTotal =
          Account.sum
            [ amountOf p
            | earlierTransaction <- earlier ++ [t],
              p <- postingsOf earlierTransaction,
              locatedValue (postingAccountName p) == declarationSettingAssetsAccountName testSettings
            ]
  ]

testSettings :: DeclarationSettings
testSettings =
  DeclarationSettings
    { declarationSettingAssetsAccountName = "assets:stripe",
      declarationSettingDomesticIncomeAccountName = "income:domestic",
      declarationSettingVATIncomeAccountName = "income:VAT",
      declarationSettingForeignIncomeAccountName = "income:foreign",
      declarationSettingFeesAccountName = "expenses:banking:stripe",
      declarationSettingRefundsAccountName = "expenses:refunds",
      declarationSettingVATExpensesAccountName = "expenses:VAT",
      declarationSettingPayoutAccountName = "assets:self-transfer:stripe-neon",
      declarationSettingOpeningAccountName = "equity:starting",
      declarationSettingDeductibleTags = ["deductible"],
      declarationSettingNotVATDeductibleTags = ["tax-deductible", "not-vat-deductible"],
      declarationSettingDocumentsDirectory = [reldir|documents/stripe|],
      declarationSettingReportsDirectory = [reldir|documents/stripe/reports|],
      declarationSettingFeesAttachmentPattern = "%Y-%m-%d_tax-invoice-%Y-%m.pdf"
    }

account :: Integer -> Account.Account
account i = case Account.fromMinimalQuantisations i of
  Nothing -> error $ unwords ["Not a valid account in this test:", show i]
  Just a -> a

exemptFeeOnly :: FeeLine
exemptFeeOnly = FeeLine "Card payments - Stripe fee" (account 2_000) Account.zero Nothing

july :: StripeMonth
july =
  StripeMonth
    { stripeMonthMonth = YearMonth 2026 7,
      stripeMonthDomesticRevenues =
        [ RevenueAtRate
            { revenueAtRateRate = 0.081,
              revenueAtRateNet = account (-100_000),
              revenueAtRateTax = account (-8_100),
              revenueAtRateRefundedNet = Account.zero,
              revenueAtRateRefundedTax = Account.zero
            }
        ],
      stripeMonthForeignRevenue = Just (account (-50_000)),
      stripeMonthRevenueWithNoTaxRow = Account.zero,
      stripeMonthFees =
        [ exemptFeeOnly,
          FeeLine "Stripe Tax - integration fee" (account 1_000) (account 81) (Just 0.081)
        ],
      stripeMonthPayouts = [StripePayout (fromGregorian 2026 7 15) (account 20_000)],
      stripeMonthOpeningBalance = account 10_000,
      stripeMonthClosingBalance = account 145_019
    }

august :: StripeMonth
august =
  StripeMonth
    { stripeMonthMonth = YearMonth 2026 8,
      stripeMonthDomesticRevenues =
        [ RevenueAtRate
            { revenueAtRateRate = 0.081,
              revenueAtRateNet = account (-100_000),
              revenueAtRateTax = account (-8_100),
              revenueAtRateRefundedNet = account 10_000,
              revenueAtRateRefundedTax = account 810
            }
        ],
      stripeMonthForeignRevenue = Nothing,
      stripeMonthRevenueWithNoTaxRow = Account.zero,
      stripeMonthFees = [],
      stripeMonthPayouts = [],
      stripeMonthOpeningBalance = account 145_019,
      stripeMonthClosingBalance = account 242_309
    }

emptyMonth :: StripeMonth
emptyMonth =
  StripeMonth
    { stripeMonthMonth = YearMonth 2026 9,
      stripeMonthDomesticRevenues = [],
      stripeMonthForeignRevenue = Nothing,
      stripeMonthRevenueWithNoTaxRow = Account.zero,
      stripeMonthFees = [],
      stripeMonthPayouts = [],
      stripeMonthOpeningBalance = Account.zero,
      stripeMonthClosingBalance = Account.zero
    }

-- | A month that gave money back without taking any, so its report is attached once.
refundsOnly :: StripeMonth
refundsOnly =
  august
    { stripeMonthDomesticRevenues =
        [ RevenueAtRate
            { revenueAtRateRate = 0.081,
              revenueAtRateNet = Account.zero,
              revenueAtRateTax = Account.zero,
              revenueAtRateRefundedNet = account 10_000,
              revenueAtRateRefundedTax = account 810
            }
        ],
      stripeMonthOpeningBalance = Account.zero,
      stripeMonthClosingBalance = account (-10_810)
    }

-- | The percentage written on each posting's ratio, so that a pair can be told from a
-- posting that merely happens to sit next to one.
ratioPercentagesOf :: Transaction () -> [Maybe String]
ratioPercentagesOf t =
  [ percentageOf p
  | Commented (Located _ p) _ <- transactionPostings t
  ]
  where
    percentageOf p = case postingRatio p of
      Nothing -> Nothing
      Just (Located _ ratioExpression) ->
        let Located _ rationalExpression = ratioExpressionRationalExpression ratioExpression
            Located _ numerator = rationalExpressionNumerator rationalExpression
         in Just (DecimalLiteral.toString numerator)

-- | Two rates in one month, each with something refunded at it.
twoRates :: [RevenueAtRate]
twoRates =
  [ RevenueAtRate
      { revenueAtRateRate = 0.026,
        revenueAtRateNet = account (-20_000),
        revenueAtRateTax = account (-520),
        revenueAtRateRefundedNet = account 1_000,
        revenueAtRateRefundedTax = account 26
      },
    RevenueAtRate
      { revenueAtRateRate = 0.081,
        revenueAtRateNet = account (-100_000),
        revenueAtRateTax = account (-8_100),
        revenueAtRateRefundedNet = account 5_000,
        revenueAtRateRefundedTax = account 405
      }
  ]
