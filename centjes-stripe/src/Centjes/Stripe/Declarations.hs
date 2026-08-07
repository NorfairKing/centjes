{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Turning monthly aggregates into the transactions that get written to a @.cent@
-- file.
--
-- The posting order here is load-bearing, and nothing in @centjes check@ enforces it.
-- @centjes-switzerland@ reads a rate off the posting that /follows/ an income or
-- expense posting, so an emitted transaction whose postings are in the wrong order
-- does not fail: it silently drops revenue or input tax from the VAT return.  The
-- golden tests in this package are the only thing standing between this module and a
-- wrong VAT return.
module Centjes.Stripe.Declarations
  ( DeclarationSettings (..),
    DeclarationError (..),
    stripeTransactions,
    monthDocumentPath,
  )
where

import qualified Centjes.Comment as Comment
import qualified Centjes.Description as Description
import Centjes.Location
import Centjes.Module
import Centjes.Report.Check (duplicateAttachmentTag)
import Centjes.Stripe.Aggregate
import Centjes.Stripe.Report
import Centjes.Validation
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Time.Calendar.Month
import Error.Diagnose
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Money.QuantisationFactor as QuantisationFactor
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Path

-- | Everything about the ledger that the emitter must not decide for itself.
data DeclarationSettings = DeclarationSettings
  { declarationSettingAssetsAccountName :: !AccountName,
    declarationSettingDomesticIncomeAccountName :: !AccountName,
    declarationSettingVATIncomeAccountName :: !AccountName,
    declarationSettingForeignIncomeAccountName :: !AccountName,
    declarationSettingFeesAccountName :: !AccountName,
    -- | Where money given back to a customer goes.
    --
    -- An expense rather than negative revenue, because @centjes-switzerland@ rejects a
    -- positive posting to an income account outright.  See 'refundsTransaction'.
    declarationSettingRefundsAccountName :: !AccountName,
    declarationSettingVATExpensesAccountName :: !AccountName,
    declarationSettingPayoutAccountName :: !AccountName,
    -- | Where the balance the importer starts from comes from.
    declarationSettingOpeningAccountName :: !AccountName,
    -- | Tags for a transaction whose expenses carried recoverable tax.
    declarationSettingDeductibleTags :: ![Tag],
    -- | Tags for a transaction whose expenses carried none.
    --
    -- A transaction tagged deductible must have at least one expense posting followed
    -- by a VAT posting, or @centjes-switzerland@ fails with
    -- @VATErrorDeductibleNoExpenses@.  A month whose every fee was exempt has none, so
    -- it needs the other pair of tags.
    declarationSettingNotVATDeductibleTags :: ![Tag],
    -- | Where the documents that have to be downloaded by hand live, relative to the
    -- ledger.
    declarationSettingDocumentsDirectory :: !(Path Rel Dir),
    -- | Where the report CSVs the importer saves live, relative to the ledger.
    declarationSettingReportsDirectory :: !(Path Rel Dir),
    -- | 'formatTime' pattern for Stripe's monthly tax invoice.
    --
    -- The only document the importer cannot fetch: Stripe's invoices for its own fees
    -- are in the dashboard under Settings, Reporting and documents, and no API serves
    -- them.  It is also the one that has to be a real invoice rather than a report,
    -- since reclaiming the input tax needs the document carrying Stripe's VAT number.
    declarationSettingFeesAttachmentPattern :: !String
  }

data DeclarationError
  = DeclarationErrorUnrepresentableAmount !QuantisationFactor !Money.Account
  | DeclarationErrorUnrepresentableRate !Rational
  | DeclarationErrorUnrepresentableDescription !Text !String
  | DeclarationErrorUnrepresentableComment !Text !String
  | DeclarationErrorUnrepresentableAttachment !String !FilePath
  | DeclarationErrorSum ![Money.Account]
  deriving stock (Show)

instance ToReport DeclarationError where
  toReport =
    let stripeErr code message hints = Err (Just code) message [] (map Hint hints)
     in \case
          DeclarationErrorUnrepresentableAmount qf account ->
            stripeErr
              "SD_UNREPRESENTABLE_AMOUNT"
              (unwords ["Cannot write", show account, "with quantisation factor", show (unQuantisationFactor qf)])
              ["The currency in the ledger is quantised more coarsely than Stripe's amounts."]
          DeclarationErrorUnrepresentableRate rate ->
            stripeErr
              "SD_UNREPRESENTABLE_RATE"
              (unwords ["Cannot write the rate", show (fromRational rate :: Double), "as a decimal"])
              []
          DeclarationErrorUnrepresentableDescription t reason ->
            stripeErr
              "SD_UNREPRESENTABLE_DESCRIPTION"
              (unwords ["Cannot write", show t, "as a description"])
              [reason]
          DeclarationErrorUnrepresentableComment t reason ->
            stripeErr
              "SD_UNREPRESENTABLE_COMMENT"
              (unwords ["Cannot write", show t, "as a comment"])
              [reason, "Stripe named a fee something that cannot be written down."]
          DeclarationErrorUnrepresentableAttachment pattern_ rendered ->
            stripeErr
              "SD_UNREPRESENTABLE_ATTACHMENT"
              (unwords ["The pattern", show pattern_, "produced", show rendered <> ",", "which is not a relative file path"])
              ["Patterns are formatTime patterns, so %Y and %m stand for the year and the month."]
          DeclarationErrorSum accounts ->
            stripeErr "SD_SUM" (unwords ["Cannot add up", show accounts]) []

-- | Every transaction the importer wants in its output file, in the order it wants
-- them written.
--
-- Within one day the order given here is the order that survives merging, and the
-- balance assertion sits on the last transaction of each month, so this order is what
-- makes the assertions true.
stripeTransactions ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  [StripeMonth] ->
  Validation DeclarationError [Transaction ()]
stripeTransactions settings quantisationFactor currency months = do
  openings <- case months of
    [] -> pure []
    (firstMonth : _)
      | stripeMonthOpeningBalance firstMonth == Account.zero -> pure []
      | otherwise ->
          (: [])
            <$> openingTransaction
              settings
              quantisationFactor
              currency
              (addDays (-1) (periodFirstDay (stripeMonthMonth firstMonth)))
              (stripeMonthOpeningBalance firstMonth)
  rest <- traverse (monthTransactions settings quantisationFactor currency) months
  pure $ openings ++ concat rest

-- | Where the balance comes from when the importer does not start at the beginning of
-- the account's life.
--
-- Taken from what Stripe says the first imported month opened with, rather than summed
-- up from anything, so the very first assertion the importer writes stands on Stripe's
-- own figure.
openingTransaction ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  Day ->
  Money.Account ->
  Validation DeclarationError (Transaction ())
openingTransaction DeclarationSettings {..} quantisationFactor currency day balance = do
  description <- requireDescription "Stripe opening balance"
  assetsPosting <- posting quantisationFactor currency declarationSettingAssetsAccountName balance
  openingPosting <- posting quantisationFactor currency declarationSettingOpeningAccountName (Account.negate balance)
  pure
    Transaction
      { transactionTimestamp = noLoc (TimestampDay day),
        transactionDescription = Just (noCommentsOn description),
        transactionPostings = [assetsPosting, openingPosting],
        transactionExtras = []
      }

monthTransactions ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  StripeMonth ->
  Validation DeclarationError [Transaction ()]
monthTransactions settings quantisationFactor currency month@StripeMonth {..} = do
  payouts <- traverse (payoutTransaction settings quantisationFactor currency) stripeMonthPayouts
  sales <- salesTransaction settings quantisationFactor currency month
  refunds <- refundsTransaction settings quantisationFactor currency month
  fees <- feesTransaction settings quantisationFactor currency month
  -- Sales and refunds are both computed from the tax summary, so in a month with both the
  -- same file is attached twice and @centjes check@ has to be told that it is deliberate.
  -- Decided here, where both transactions are in hand, so that the claim cannot be made in
  -- a month that emits only one of them.
  let (salesTagged, refundsTagged) = case (sales, refunds) of
        (Just salesTransaction', Just refundsTransaction') ->
          ( Just (tagDuplicateAttachment salesTransaction'),
            Just (tagDuplicateAttachment refundsTransaction')
          )
        _ -> (sales, refunds)
  let transactions = payouts ++ maybeToList salesTagged ++ maybeToList refundsTagged ++ maybeToList fees
  -- The month's balance is asserted on whichever transaction comes last, so that there
  -- is exactly one assertion per month and it stands after everything that month moved.
  case reverse transactions of
    [] -> pure []
    (lastTransaction : earlier) -> do
      assertion <- balanceAssertion settings quantisationFactor currency stripeMonthClosingBalance
      pure $
        reverse $
          lastTransaction {transactionExtras = transactionExtras lastTransaction ++ [assertion]} : earlier

-- | One transaction per payout, dated the day the money left the Stripe balance.
--
-- Not one per month: the other leg of each payout is a single line on a bank statement,
-- and the bank importer books those one at a time.
payoutTransaction ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  StripePayout ->
  Validation DeclarationError (Transaction ())
payoutTransaction DeclarationSettings {..} quantisationFactor currency StripePayout {..} = do
  description <- requireDescription $ Text.pack $ unwords ["Stripe payout", showGregorian stripePayoutDay]
  payoutPosting <- posting quantisationFactor currency declarationSettingPayoutAccountName stripePayoutAmount
  assetsPosting <- posting quantisationFactor currency declarationSettingAssetsAccountName (Account.negate stripePayoutAmount)
  pure
    Transaction
      { transactionTimestamp = noLoc (TimestampDay stripePayoutDay),
        transactionDescription = Just (noCommentsOn description),
        transactionPostings = [payoutPosting, assetsPosting],
        transactionExtras = []
      }

-- | The month's revenue, booked gross, dated the last day of the month.
--
-- Every domestic income posting is immediately followed by its VAT posting, because
-- that is the only shape @centjes-switzerland@ reads a rate from: it pairs each posting
-- with its successor.  Two rates in one month are therefore two adjacent pairs rather
-- than one pair with two rates, and the foreign revenue goes after all of the pairs so
-- that it never lands inside one.
salesTransaction ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  StripeMonth ->
  Validation DeclarationError (Maybe (Transaction ()))
salesTransaction DeclarationSettings {..} quantisationFactor currency StripeMonth {..} =
  case (filter ((/= Account.zero) . revenueAtRateNet) stripeMonthDomesticRevenues, stripeMonthForeignRevenue) of
    ([], Nothing) -> pure Nothing
    (domestic, mForeign) -> do
      description <- requireDescription $ Text.pack $ unwords ["Stripe sales", show stripeMonthMonth]
      domesticPostings <-
        concat
          <$> traverse
            ( \RevenueAtRate {..} -> do
                netPosting <- posting quantisationFactor currency declarationSettingDomesticIncomeAccountName revenueAtRateNet
                vatPosting <-
                  ratioPosting
                    quantisationFactor
                    currency
                    declarationSettingVATIncomeAccountName
                    revenueAtRateTax
                    revenueAtRateRate
                pure [netPosting, vatPosting]
            )
            domestic
      foreignPostings <-
        traverse
          (posting quantisationFactor currency declarationSettingForeignIncomeAccountName)
          (maybeToList mForeign)
      total <-
        requireSum $
          concat
            [ map revenueAtRateNet domestic,
              map revenueAtRateTax domestic,
              maybeToList mForeign
            ]
      assetsPosting <- posting quantisationFactor currency declarationSettingAssetsAccountName (Account.negate total)
      -- The tax summary is where the country split comes from and the activity summary
      -- is where the total comes from, so both are what this transaction rests on.
      attachments <-
        traverse
          (reportAttachment declarationSettingReportsDirectory stripeMonthMonth)
          [ReportKindTax, ReportKindActivity]
      pure $
        Just
          Transaction
            { transactionTimestamp = noLoc (TimestampDay (periodLastDay stripeMonthMonth)),
              transactionDescription = Just (noCommentsOn description),
              transactionPostings = concat [domesticPostings, foreignPostings, [assetsPosting]],
              transactionExtras = attachments
            }

-- | What was given back to customers, as an expense.
--
-- An expense rather than negative revenue because @centjes-switzerland@ requires every
-- posting to an income account to be negative and fails outright on a positive one, so
-- a refund cannot be written as the reversal it really is.
--
-- The consequence to know about: this puts the refund on the wrong two lines of a Swiss
-- return.  Turnover stays gross where it should be net of credits, and input tax is
-- higher than it was.  The VAT actually payable comes out right, because reducing output
-- tax and increasing input tax by the same amount have the same effect, but a return
-- filed from this overstates two figures.
refundsTransaction ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  StripeMonth ->
  Validation DeclarationError (Maybe (Transaction ()))
refundsTransaction DeclarationSettings {..} quantisationFactor currency StripeMonth {..} =
  case filter ((/= Account.zero) . revenueAtRateRefundedNet) stripeMonthDomesticRevenues of
    [] -> pure Nothing
    refunded -> do
      description <- requireDescription $ Text.pack $ unwords ["Stripe refunds", show stripeMonthMonth]
      refundPostings <-
        concat
          <$> traverse
            ( \RevenueAtRate {..} -> do
                refundPosting <-
                  posting quantisationFactor currency declarationSettingRefundsAccountName revenueAtRateRefundedNet
                if revenueAtRateRefundedTax == Account.zero
                  then pure [refundPosting]
                  else do
                    vatPosting <-
                      ratioPosting
                        quantisationFactor
                        currency
                        declarationSettingVATExpensesAccountName
                        revenueAtRateRefundedTax
                        revenueAtRateRate
                    pure [refundPosting, vatPosting]
            )
            refunded
      total <-
        requireSum $
          concat
            [ map revenueAtRateRefundedNet refunded,
              map revenueAtRateRefundedTax refunded
            ]
      assetsPosting <- posting quantisationFactor currency declarationSettingAssetsAccountName (Account.negate total)
      -- The tax summary again: the report that says what was sold is the one that says
      -- what was given back.  Whether that ends up being the same file twice is decided by
      -- 'monthTransactions', which is where both transactions are in hand.
      attachment <- reportAttachment declarationSettingReportsDirectory stripeMonthMonth ReportKindTax
      let carriedTax = any ((/= Account.zero) . revenueAtRateRefundedTax) refunded
      pure $
        Just
          Transaction
            { transactionTimestamp = noLoc (TimestampDay (periodLastDay stripeMonthMonth)),
              transactionDescription = Just (noCommentsOn description),
              transactionPostings = refundPostings ++ [assetsPosting],
              transactionExtras = attachment : tagExtras (tagsFor DeclarationSettings {..} carriedTax)
            }

-- | The month's Stripe fees, one posting per fee Stripe named, each followed by the VAT
-- on it where there was any.
--
-- Card processing is an exempt financial service and carries none, so its posting is
-- followed by the next fee instead.  That is not a mistake: on a deductible transaction
-- the VAT report silently ignores an expense posting whose successor is not the VAT
-- posting, which is exactly right for a fee that had no VAT to reclaim.
feesTransaction ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  StripeMonth ->
  Validation DeclarationError (Maybe (Transaction ()))
feesTransaction DeclarationSettings {..} quantisationFactor currency StripeMonth {..} =
  case stripeMonthFees of
    [] -> pure Nothing
    fees -> do
      description <- requireDescription $ Text.pack $ unwords ["Stripe fees", show stripeMonthMonth]
      feePostings <-
        concat
          <$> traverse
            ( \FeeLine {..} -> do
                Commented lp _ <- posting quantisationFactor currency declarationSettingFeesAccountName feeLineAmount
                comment <- requireComment feeLineDescription
                let feePosting = Commented lp (Just (noLoc comment))
                case feeLineRate of
                  Nothing -> pure [feePosting]
                  Just rate -> do
                    vatPosting <-
                      ratioPosting
                        quantisationFactor
                        currency
                        declarationSettingVATExpensesAccountName
                        feeLineTax
                        rate
                    pure [feePosting, vatPosting]
            )
            fees
      total <- requireSum $ map feeLineAmount fees ++ map feeLineTax fees
      assetsPosting <- posting quantisationFactor currency declarationSettingAssetsAccountName (Account.negate total)
      -- Stripe's own tax invoice, which is what the input tax is reclaimed on, and the
      -- fee summary the amounts were read from.
      taxInvoice <-
        attachmentExtra declarationSettingDocumentsDirectory declarationSettingFeesAttachmentPattern stripeMonthMonth Nothing
      feeSummary <- reportAttachment declarationSettingReportsDirectory stripeMonthMonth ReportKindFees
      let carriedTax = not (null (mapMaybe feeLineRate fees))
      pure $
        Just
          Transaction
            { transactionTimestamp = noLoc (TimestampDay (periodLastDay stripeMonthMonth)),
              transactionDescription = Just (noCommentsOn description),
              transactionPostings = feePostings ++ [assetsPosting],
              transactionExtras =
                concat
                  [ [taxInvoice, feeSummary],
                    tagExtras (tagsFor DeclarationSettings {..} carriedTax)
                  ]
            }

-- | Which tags an expense transaction gets.
--
-- A transaction tagged deductible must have at least one expense posting followed by a
-- VAT posting, so a month whose expenses all carried exempt fees gets the other pair
-- instead of an error out of the VAT report.
tagsFor :: DeclarationSettings -> Bool -> [Tag]
tagsFor DeclarationSettings {..} carriedTax =
  if carriedTax
    then declarationSettingDeductibleTags
    else declarationSettingNotVATDeductibleTags

tagExtras :: [Tag] -> [Commented () (TransactionExtra ())]
tagExtras tags = [noCommentsOn (TransactionTag (noLoc (ExtraTag (noLoc tag)))) | tag <- tags]

balanceAssertion ::
  DeclarationSettings ->
  QuantisationFactor ->
  CurrencySymbol ->
  Money.Account ->
  Validation DeclarationError (Commented () (TransactionExtra ()))
balanceAssertion DeclarationSettings {..} quantisationFactor currency balance = do
  literal <- requireLiteral quantisationFactor balance
  pure $
    noCommentsOn $
      TransactionAssertion $
        noLoc $
          ExtraAssertion $
            noLoc $
              AssertionEquals
                AssertionScopeReal
                (noLoc declarationSettingAssetsAccountName)
                (noLoc literal)
                (noLoc (CommodityExpressionCurrency (noLoc currency)))

-- | The report CSV the importer saved for a month, as an attachment.
reportAttachment ::
  Path Rel Dir ->
  Month ->
  ReportKind ->
  Validation DeclarationError (Commented () (TransactionExtra ()))
reportAttachment reportsDirectory month kind =
  attachmentExtra reportsDirectory (reportKindAttachmentPattern kind) month Nothing

-- | Say that attaching the same file twice was meant.
--
-- @centjes check@ refuses the same file on two transactions without this, which is a good
-- rule: it is normally a copy-paste mistake.  Here it is not, because one report says both
-- what was sold and what was given back.
tagDuplicateAttachment :: Transaction () -> Transaction ()
tagDuplicateAttachment t =
  t {transactionExtras = transactionExtras t ++ tagExtras [duplicateAttachmentTag]}

attachmentExtra ::
  Path Rel Dir ->
  String ->
  Month ->
  Maybe Text ->
  Validation DeclarationError (Commented () (TransactionExtra ()))
attachmentExtra documentsDirectory pattern_ month mCommentText = do
  relFile <- monthDocumentPath documentsDirectory pattern_ month
  mComment <- traverse requireComment mCommentText
  pure $
    Commented
      ( noLoc $
          TransactionAttachment $
            noLoc $
              ExtraAttachment $
                noLoc $
                  Attachment $
                    noLoc relFile
      )
      (noLoc <$> mComment)

-- | Where a month's document belongs, relative to the ledger.
--
-- The importer both writes files here and refers to them from the ledger, so this is
-- the one place that decides what a month's document is called.
monthDocumentPath ::
  Path Rel Dir ->
  String ->
  Month ->
  Validation DeclarationError (Path Rel File)
monthDocumentPath directory pattern_ month = do
  let rendered = formatTime defaultTimeLocale pattern_ (periodLastDay month)
  case parseRelFile rendered of
    Nothing -> validationFailure $ DeclarationErrorUnrepresentableAttachment pattern_ rendered
    Just relFile -> pure (directory </> relFile)

posting ::
  QuantisationFactor ->
  CurrencySymbol ->
  AccountName ->
  Money.Account ->
  Validation DeclarationError (Commented () (Posting ()))
posting quantisationFactor currency accountName account = do
  literal <- requireLiteral quantisationFactor account
  pure $
    noCommentsOn
      Posting
        { postingReal = True,
          postingAccountName = noLoc accountName,
          postingAccount = noLoc literal,
          postingCurrencySymbol = noLoc currency,
          postingPrice = Nothing,
          postingRatio = Nothing
        }

-- | A VAT posting, carrying the rate as an exclusive percentage ratio.
--
-- Exclusive because Stripe's amounts are net of tax, and a percentage because
-- @centjes-switzerland@ only accepts a percentage there.  The ratio is never checked
-- arithmetically by centjes, which is what lets the posted tax be what Stripe actually
-- collected rather than the rate applied to the month's total.  The collected amount is
-- the number that has to be paid over, so it is the defensible one.
ratioPosting ::
  QuantisationFactor ->
  CurrencySymbol ->
  AccountName ->
  Money.Account ->
  Rational ->
  Validation DeclarationError (Commented () (Posting ()))
ratioPosting quantisationFactor currency accountName account rate = do
  Commented (Located _ p) _ <- posting quantisationFactor currency accountName account
  rateLiteral <- case DecimalLiteral.fromRational (rate * 100) of
    Nothing -> validationFailure $ DeclarationErrorUnrepresentableRate rate
    Just rateLiteral -> pure rateLiteral
  let ratioExpression =
        RatioExpression
          { ratioExpressionInclusive = Just False,
            ratioExpressionRounding = Nothing,
            ratioExpressionRationalExpression =
              noLoc
                RationalExpression
                  { rationalExpressionNumerator = noLoc rateLiteral,
                    rationalExpressionDenominator = Nothing,
                    rationalExpressionPercent = True
                  }
          }
  pure $ noCommentsOn p {postingRatio = Just (noLoc ratioExpression)}

-- | Write an amount the way @centjes format@ would, so that formatting the output file
-- does not immediately change it again.
requireLiteral :: QuantisationFactor -> Money.Account -> Validation DeclarationError DecimalLiteral
requireLiteral quantisationFactor account =
  case Account.toDecimalLiteral quantisationFactor account of
    Nothing -> validationFailure $ DeclarationErrorUnrepresentableAmount quantisationFactor account
    Just literal ->
      pure $
        DecimalLiteral.setSignRequired $
          DecimalLiteral.setMinimumDigits (QuantisationFactor.digits quantisationFactor) literal

requireSum :: [Money.Account] -> Validation DeclarationError Money.Account
requireSum accounts = case Account.sum accounts of
  Nothing -> validationFailure $ DeclarationErrorSum accounts
  Just total -> pure total

requireDescription :: Text -> Validation DeclarationError Description
requireDescription t = case Description.fromText t of
  Left reason -> validationFailure $ DeclarationErrorUnrepresentableDescription t reason
  Right description -> pure description

requireComment :: Text -> Validation DeclarationError Comment
requireComment t = case Comment.fromText t of
  Left reason -> validationFailure $ DeclarationErrorUnrepresentableComment t reason
  Right comment -> pure comment
