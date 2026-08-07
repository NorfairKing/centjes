{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Stripe.ReportSpec (spec) where

import Centjes.Stripe.Gen (codecSpec)
import Centjes.Stripe.Report
import Centjes.Stripe.Timestamp
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Money.Account as Account
import Money.QuantisationFactor (QuantisationFactor (..))
import Test.Syd

spec :: Spec
spec = do
  codecSpec @ReportTypeId
  codecSpec @ReportType

  describe "reportTypeCoversThrough" $ do
    let reportType =
          ReportType
            { reportTypeId = ReportTypeId "balance.summary.1",
              reportTypeDataAvailableStart = StripeTimestamp 0,
              reportTypeDataAvailableEnd = StripeTimestamp 1_785_542_400
            }
    it "covers a moment it has data through" $
      reportTypeCoversThrough (StripeTimestamp 1_785_542_400) reportType `shouldBe` True

    -- A month is not reported on merely because it is over: fee data lags days behind.
    it "does not cover a moment past its data" $
      reportTypeCoversThrough (StripeTimestamp 1_785_542_401) reportType `shouldBe` False

  describe "parseReportTable" $ do
    it "reads the header as the columns" $
      case parseReportTable "\"a\",\"b\"\n\"1\",\"2\"\n" of
        Left e -> expectationFailure (renderReportError e)
        Right table -> reportTableColumns table `shouldBe` [ReportColumn "a", ReportColumn "b"]

    it "reads a header with no rows under it" $
      case parseReportTable "\"suite\",\"amount\"\n" of
        Left e -> expectationFailure (renderReportError e)
        Right table -> length (reportTableRows table) `shouldBe` 0

    it "refuses something that is not a CSV" $
      case parseReportTable "" of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected empty input to be refused"

  describe "requireColumn" $ do
    it "finds a column by name rather than by position" $
      withTable "\"b\",\"a\"\n\"second\",\"first\"\n" $ \table row ->
        requireColumn table (ReportColumn "a") row `shouldBe` Right "first"

    -- The importer names the columns it wants when it asks for the report, so a column
    -- that is missing means Stripe renamed or dropped it, and that has to be said out
    -- loud rather than read as something else.
    it "names the column it wanted and the ones there are" $
      withTable "\"a\"\n\"1\"\n" $ \table row ->
        case requireColumn table (ReportColumn "nope") row of
          Left e@(ReportErrorNoSuchColumn column available) -> do
            column `shouldBe` ReportColumn "nope"
            available `shouldBe` [ReportColumn "a"]
            renderReportError e `shouldSatisfy` Text.isInfixOf "nope" . Text.pack
          other -> expectationFailure $ unwords ["Expected a missing column, got", show other]

  describe "requireAmount" $ do
    -- The tax and balance reports use two decimal places.
    it "reads two decimal places" $
      withTable "\"x\"\n\"134.01\"\n" $ \table row ->
        requireAmount chf table (ReportColumn "x") row `shouldBe` Right (account 13401)

    -- The fee report uses eighteen, for the same kind of number.
    it "reads eighteen decimal places" $
      withTable "\"x\"\n\"9.860000000000000000\"\n" $ \table row ->
        requireAmount chf table (ReportColumn "x") row `shouldBe` Right (account 986)

    it "reads a negative amount" $
      withTable "\"x\"\n\"-10.32\"\n" $ \table row ->
        requireAmount chf table (ReportColumn "x") row `shouldBe` Right (account (-1032))

    it "reads a zero written out to eighteen places" $
      withTable "\"x\"\n\"0.000000000000000000\"\n" $ \table row ->
        requireAmount chf table (ReportColumn "x") row `shouldBe` Right Account.zero

    -- Silently rounding money is how a reconciliation stops meaning anything.
    it "refuses an amount the currency cannot hold exactly" $
      withTable "\"x\"\n\"1.005\"\n" $ \table row ->
        case requireAmount chf table (ReportColumn "x") row of
          Left (ReportErrorNotRepresentable {}) -> pure ()
          other -> expectationFailure $ unwords ["Expected a refusal, got", show other]

    it "refuses something that is not a number" $
      withTable "\"x\"\n\"n/a\"\n" $ \table row ->
        case requireAmount chf table (ReportColumn "x") row of
          Left (ReportErrorNotADecimal {}) -> pure ()
          other -> expectationFailure $ unwords ["Expected a refusal, got", show other]

  describe "requireRational" $
    it "reads a tax rate as the fraction it is" $
      withTable "\"tax_rate\"\n\"0.081\"\n" $ \table row ->
        requireRational table (ReportColumn "tax_rate") row `shouldBe` Right (81 / 1000)

  describe "columnOrEmpty" $
    -- The balance report leaves the currency off its payout rows entirely.
    it "reads a field Stripe left blank as empty" $
      withTable "\"category\",\"currency\"\n\"payouts\",\n" $ \_ row ->
        columnOrEmpty (ReportColumn "currency") row `shouldBe` ""

withTable :: LB.ByteString -> (ReportTable -> ReportRow -> IO ()) -> IO ()
withTable contents func = case parseReportTable contents of
  Left e -> expectationFailure (renderReportError e)
  Right table -> case reportTableRows table of
    [] -> expectationFailure "Expected a row"
    (row : _) -> func table row

chf :: QuantisationFactor
chf = QuantisationFactor 100

account :: Integer -> Account.Account
account i = case Account.fromMinimalQuantisations i of
  Nothing -> error $ unwords ["Not a valid account in this test:", show i]
  Just a -> a
