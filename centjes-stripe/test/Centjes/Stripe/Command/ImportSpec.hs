{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Centjes.Stripe.Command.ImportSpec (spec) where

import Centjes.Format (formatModule)
import Centjes.Location
import Centjes.Merge (mergeTransactionDeclarations)
import Centjes.Module
import Centjes.Parse (parseModule)
import Centjes.Parse.TestUtils (shouldParse)
import Centjes.Stripe.Aggregate
import Centjes.Stripe.Command.Import
import Centjes.Stripe.Report
import Centjes.Stripe.Timestamp
import Conduit
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.Month
import qualified Money.Account as Account
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec = do
  -- The output file as the importer first wrote it, which most of these tests start from.
  let twoMonths :: Text
      twoMonths =
        Text.unlines
          [ "2026-07-31",
            "  | Stripe sales 2026-07",
            "  * income:foreign -370.00 CHF",
            "  * assets:stripe  +370.00 CHF",
            "",
            "2026-08-31",
            "  | Stripe sales 2026-08",
            "  * income:foreign -520.00 CHF",
            "  * assets:stripe  +520.00 CHF"
          ]
  let moduleOf :: Path Abs Dir -> Text -> IO (Module ())
      moduleOf here contents = stripModuleAnnotation <$> shouldParse parseModule here [relfile|stripe.cent|] contents
  let declarationsOf :: Path Abs Dir -> Text -> IO [GenLocated () (Declaration ())]
      declarationsOf here contents = moduleDeclarations <$> moduleOf here contents
  let transactionsOf :: Path Abs Dir -> Text -> IO [Transaction ()]
      transactionsOf here contents = do
        declarations <- declarationsOf here contents
        pure (map locatedValue (declarationsTransactions (splitDeclarations declarations)))

  describe "monthsToImport" $ do
    let reportsThrough t =
          [ ReportType
              { reportTypeId = ReportTypeId "balance.summary.1",
                reportTypeDataAvailableStart = StripeTimestamp 0,
                reportTypeDataAvailableEnd = StripeTimestamp t
              }
          ]
    -- 2026-08-01T00:00:00Z, which is when July becomes reportable.
    let augustFirst = 1_785_542_400

    it "imports a month that has ended and been reported on" $
      fst (monthsToImport (YearMonth 2026 7) (fromGregorian 2026 8 6) S.empty (reportsThrough augustFirst))
        `shouldBe` [YearMonth 2026 7]

    -- Stripe is still collecting the month it is in the middle of.
    it "never imports the current month" $
      fst (monthsToImport (YearMonth 2026 8) (fromGregorian 2026 8 6) S.empty (reportsThrough (augustFirst * 2)))
        `shouldBe` []

    -- Fee data lags days behind the rest, so a month can be over and still not reported
    -- on.  This is Stripe's own answer rather than a guess from today's date.
    it "holds back a month Stripe has not finished computing" $ do
      let (wanted, held) =
            monthsToImport (YearMonth 2026 7) (fromGregorian 2026 8 6) S.empty (reportsThrough (augustFirst - 1))
      wanted `shouldBe` []
      map fst held `shouldBe` [YearMonth 2026 7]
      map snd held `shouldSatisfy` any (Text.isInfixOf "not finished computing" . Text.pack)

    it "holds back a month the file already has" $ do
      let (wanted, held) =
            monthsToImport
              (YearMonth 2026 7)
              (fromGregorian 2026 8 6)
              (S.singleton (YearMonth 2026 7))
              (reportsThrough augustFirst)
      wanted `shouldBe` []
      map snd held `shouldSatisfy` any (Text.isInfixOf "never rewrites" . Text.pack)

    it "imports several months at once when several are due" $
      fst (monthsToImport (YearMonth 2026 5) (fromGregorian 2026 8 6) S.empty (reportsThrough augustFirst))
        `shouldBe` [YearMonth 2026 5, YearMonth 2026 6, YearMonth 2026 7]

    it "has nothing to say before its first month" $
      fst (monthsToImport (YearMonth 2027 1) (fromGregorian 2026 8 6) S.empty (reportsThrough augustFirst))
        `shouldBe` []

  -- Each month asserts the balance Stripe closed it on, which only holds if every month
  -- between the opening balance and it was booked.  So months go in as a contiguous run
  -- or not at all.
  describe "untilEvidenceMissing" $ do
    -- An empty month, which is all this stage looks at beyond its month.
    let monthOf :: Integer -> Int -> StripeMonth
        monthOf year month =
          StripeMonth
            { stripeMonthMonth = YearMonth year month,
              stripeMonthDomesticRevenues = [],
              stripeMonthForeignRevenue = Nothing,
              stripeMonthRevenueWithNoTaxRow = Account.zero,
              stripeMonthFees = [],
              stripeMonthPayouts = [],
              stripeMonthOpeningBalance = Account.zero,
              stripeMonthClosingBalance = Account.zero
            }
    let monthsThrough :: [(StripeMonth, Maybe (Path Rel File))] -> IO [Month]
        monthsThrough months =
          runNoLoggingT $
            runConduit $
              yieldMany months .| untilEvidenceMissing .| mapC stripeMonthMonth .| sinkList

    it "passes on every month whose evidence is there" $ do
      through <- monthsThrough [(monthOf 2026 6, Nothing), (monthOf 2026 7, Nothing)]
      through `shouldBe` [YearMonth 2026 6, YearMonth 2026 7]

    it "stops at a month whose evidence is missing" $ do
      through <- monthsThrough [(monthOf 2026 6, Nothing), (monthOf 2026 7, Just [relfile|tax-invoice.pdf|])]
      through `shouldBe` [YearMonth 2026 6]

    -- The gap is the thing to avoid: writing August while July is held would leave
    -- August's assertion short by everything that moved in July, and the importer never
    -- rewrites a month, so July could then only be filled in by hand.  Stopping rather
    -- than skipping also means August is never fetched.
    it "stops rather than skipping, so nothing after a held-back month gets through" $ do
      through <-
        monthsThrough
          [ (monthOf 2026 6, Nothing),
            (monthOf 2026 7, Just [relfile|tax-invoice.pdf|]),
            (monthOf 2026 8, Nothing)
          ]
      through `shouldBe` [YearMonth 2026 6]

    it "passes nothing on when the very first month is held back" $ do
      through <- monthsThrough [(monthOf 2026 6, Just [relfile|tax-invoice.pdf|]), (monthOf 2026 7, Nothing)]
      through `shouldBe` []

  describe "monthsAlreadyImported" $ do
    it "has nothing in an empty file" $
      monthsAlreadyImported [] `shouldBe` S.empty

    it "is the month of every transaction already in the file" $ do
      here <- getCurrentDir
      declarations <- declarationsOf here twoMonths
      monthsAlreadyImported declarations `shouldBe` S.fromList [YearMonth 2026 7, YearMonth 2026 8]

    it "ignores declarations that are not transactions" $ do
      here <- getCurrentDir
      declarations <- declarationsOf here "-- A comment\n\naccount assets:stripe\n"
      monthsAlreadyImported declarations `shouldBe` S.empty

  -- The file is the source of truth.  Stripe is only where a first draft of a month comes
  -- from; what the user has turned that month into is what that month is.
  describe "transactionsToAdd" $ do
    it "adds nothing that the file already has a month for" $ do
      here <- getCurrentDir
      declarations <- declarationsOf here twoMonths
      transactions <- transactionsOf here twoMonths
      transactionsToAdd (monthsAlreadyImported declarations) transactions `shouldSatisfy` null

    -- Keyed by date, not by description, so renaming a transaction does not make the
    -- importer add its month again.
    it "leaves a month alone whose transaction was renamed by hand" $ do
      let renamed =
            Text.unlines
              [ "2026-07-31",
                "  | July, renamed by hand",
                "  * income:foreign -370.00 CHF",
                "  * assets:stripe  +370.00 CHF"
              ]
      here <- getCurrentDir
      declarations <- declarationsOf here renamed
      transactions <- transactionsOf here twoMonths
      map transactionMonth (transactionsToAdd (monthsAlreadyImported declarations) transactions) `shouldBe` [YearMonth 2026 8]

  describe "rewriting the output file" $ do
    it "writes back exactly what it wrote the first time" $ do
      here <- getCurrentDir
      existing <- moduleOf here twoMonths
      formatModule (mergeTransactionDeclarations existing []) `shouldBe` twoMonths

    -- Adding a month must not cost the file anything it already said.  The account
    -- declarations are the user's, written by hand, and the importer only ever adds to
    -- this file.
    it "keeps every declaration that is not a transaction" $ do
      let declaredAccounts =
            Text.unlines
              [ "account assets:stripe",
                "  + assert currency CHF",
                "account expenses:banking:stripe",
                "",
                "2026-07-31",
                "  | Stripe sales 2026-07",
                "  * income:foreign -370.00 CHF",
                "  * assets:stripe  +370.00 CHF"
              ]
      here <- getCurrentDir
      existing <- moduleOf here declaredAccounts
      transactions <- transactionsOf here twoMonths
      let merged =
            mergeTransactionDeclarations
              existing
              (map noLoc (transactionsToAdd (monthsAlreadyImported (moduleDeclarations existing)) transactions))
      [ accountName
        | Located _ accountDeclaration <- declarationsAccounts (splitDeclarations (moduleDeclarations merged)),
          let Located _ accountName = accountDeclarationName accountDeclaration
        ]
        `shouldBe` ["assets:stripe", "expenses:banking:stripe"]

    -- An import in this file is the user's too, and there is no reason it should cost
    -- them one to add a month.
    it "keeps an import the file already had" $ do
      let withAnImport =
            Text.unlines
              [ "import stripe-by-hand",
                "",
                "account assets:stripe",
                "  + assert currency CHF"
              ]
      here <- getCurrentDir
      existing <- moduleOf here withAnImport
      transactions <- transactionsOf here twoMonths
      let merged = mergeTransactionDeclarations existing (map noLoc transactions)
      map (locatedValue . importFile . locatedValue) (moduleImports merged) `shouldBe` [[relfile|stripe-by-hand|]]

    -- The whole point: a month the user has rewritten stays rewritten.
    it "leaves a hand-edited month exactly as the user left it" $ do
      let handEdited =
            Text.unlines
              [ "2026-07-31",
                "  | Stripe sales 2026-07 (split by hand)",
                "  * income:domestic -340.00 CHF",
                "  * income:VAT       -27.54 CHF ~e 8.1%",
                "  * income:foreign   -30.00 CHF",
                "  * assets:stripe   +397.54 CHF"
              ]
      here <- getCurrentDir
      existing <- moduleOf here handEdited
      transactions <- transactionsOf here handEdited
      formatModule
        ( mergeTransactionDeclarations
            existing
            (map noLoc (transactionsToAdd (monthsAlreadyImported (moduleDeclarations existing)) transactions))
        )
        `shouldBe` handEdited
