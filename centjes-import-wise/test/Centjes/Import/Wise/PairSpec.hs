{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Centjes.Import.Wise.PairSpec (spec) where

import Centjes.CurrencySymbol (CurrencySymbol (..))
import Centjes.Import.Wise.Pair
import Centjes.Import.Wise.Statement
import Data.List (isInfixOf)
import Data.Time.Calendar (fromGregorian)
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Test.Syd

spec :: Spec
spec = do
  describe "pairRows" $ do
    it "leaves an ordinary payment alone" $
      pairRows [payment] `shouldBe` Right [WiseMovement payment]

    -- The whole point of reading more than one statement at a time: each side
    -- of a conversion on its own looks like money appearing or vanishing.
    it "puts the two sides of a conversion back together" $
      pairRows [conversionOut, conversionBack]
        `shouldBe` Right [WiseConversion conversionOut conversionBack]

    it "puts them together whichever statement was read first" $
      pairRows [conversionBack, conversionOut]
        `shouldBe` Right [WiseConversion conversionOut conversionBack]

    -- Booking one side alone would leave the balance it moved to short by the
    -- whole conversion, and its running balance assertion would be the thing
    -- that eventually said so, long after the import.
    it "refuses a conversion whose other side was not read" $
      case pairRows [conversionOut] of
        Right events -> expectationFailure $ unwords ["Expected a failure, got:", show events]
        Left errs -> map renderPairError errs `shouldSatisfy` any ("CHF" `isInfixOf`)

    it "names the currency whose statement is missing from the other side too" $
      case pairRows [conversionBack] of
        Right events -> expectationFailure $ unwords ["Expected a failure, got:", show events]
        Left errs -> map renderPairError errs `shouldSatisfy` any ("EUR" `isInfixOf`)

    -- Passing the same statement twice would otherwise double every
    -- transaction in it, and the ledger would balance while being wrong.
    it "refuses the same row twice" $
      case pairRows [payment, payment] of
        Right events -> expectationFailure $ unwords ["Expected a failure, got:", show events]
        Left errs -> map renderPairError errs `shouldSatisfy` any ("twice" `isInfixOf`)

    it "refuses a conversion with more sides than two" $
      case pairRows [conversionOut, conversionBack, conversionOut {rowCurrency = CurrencySymbol "USD"}] of
        Right events -> expectationFailure $ unwords ["Expected a failure, got:", show events]
        Left errs -> length errs `shouldBe` 1

    -- Two statements are read into one list, so what comes out has to be put
    -- back in order, however the statements were passed in.
    it "puts what it read in date order" $
      map wiseEventId (expectPaired (pairRows [laterPayment, conversionBack, payment, conversionOut]))
        `shouldBe` ["PAYMENT-1", "CONVERSION-1", "PAYMENT-2"]

    -- The date column does not say which of two things on a day came first, but
    -- the statement they are both in lists them in the order they happened, and
    -- the running balance it states only adds up in that order.
    it "keeps the order a statement lists two things on one day in" $
      let second' = payment {rowId = "SECOND", rowRunningBalance = Just (literal "1456.00")}
          first' = payment {rowId = "FIRST"}
       in map wiseEventId (expectPaired (pairRows [first', second'])) `shouldBe` ["FIRST", "SECOND"]

    it "keeps it even when the ids would sort the other way" $
      let second' = payment {rowId = "A", rowRunningBalance = Just (literal "1456.00")}
          first' = payment {rowId = "B"}
       in map wiseEventId (expectPaired (pairRows [first', second'])) `shouldBe` ["B", "A"]

    -- Two balances say nothing about each other's order, so something has to
    -- decide, and it has to be the same thing every run.
    it "orders two statements' rows on the same day by id" $
      let chf = payment {rowId = "B", rowCurrency = CurrencySymbol "CHF"}
          eur = payment {rowId = "A"}
       in map wiseEventId (expectPaired (pairRows [chf, eur])) `shouldBe` ["A", "B"]

    -- The money that paid for a conversion is in the same statement as the
    -- conversion, before it.  Ordering by id instead would put the conversion
    -- first and leave every assertion after it short by the payment.
    it "puts a payment before the conversion it paid for, on the same day" $
      let funding = payment {rowId = "ZZZ-FUNDING", rowDate = rowDate conversionOut}
       in map wiseEventId (expectPaired (pairRows [funding, conversionOut, conversionBack]))
            `shouldBe` ["ZZZ-FUNDING", "CONVERSION-1"]

    it "has nothing to say about no statements at all" $
      pairRows [] `shouldBe` Right []

  describe "wiseEventDay" $
    it "dates a conversion by the side money left" $
      wiseEventDay (WiseConversion conversionOut conversionBack {rowDate = fromGregorian 2025 2 1})
        `shouldBe` fromGregorian 2025 1 15

expectPaired :: Either [PairError] [WiseEvent] -> [WiseEvent]
expectPaired = \case
  Left errs -> error $ unlines ("Expected these rows to pair up:" : map renderPairError errs)
  Right events -> events

payment :: Row
payment =
  Row
    { rowId = "PAYMENT-1",
      rowDate = fromGregorian 2025 1 3,
      rowAmount = literal "50.00",
      rowCurrency = CurrencySymbol "EUR",
      rowDescription = "Received money from A Friend",
      rowPaymentReference = "",
      rowMerchant = "",
      rowRunningBalance = Just (literal "1406.00"),
      rowExchangeFrom = Nothing,
      rowExchangeTo = Nothing,
      rowTotalFees = Just (literal "0.00")
    }

laterPayment :: Row
laterPayment =
  payment
    { rowId = "PAYMENT-2",
      rowDate = fromGregorian 2025 1 20
    }

conversionOut :: Row
conversionOut =
  Row
    { rowId = "CONVERSION-1",
      rowDate = fromGregorian 2025 1 15,
      rowAmount = literal "-9600.15",
      rowCurrency = CurrencySymbol "EUR",
      rowDescription = "Converted 9559.05 EUR to 8983.40 CHF",
      rowPaymentReference = "",
      rowMerchant = "",
      rowRunningBalance = Just (literal "0.00"),
      rowExchangeFrom = Just (CurrencySymbol "EUR"),
      rowExchangeTo = Just (CurrencySymbol "CHF"),
      rowTotalFees = Just (literal "41.10")
    }

conversionBack :: Row
conversionBack =
  conversionOut
    { rowAmount = literal "8983.40",
      rowCurrency = CurrencySymbol "CHF",
      rowRunningBalance = Just (literal "8983.40"),
      rowTotalFees = Just (literal "0.00")
    }

literal :: String -> DecimalLiteral.DecimalLiteral
literal s = case DecimalLiteral.fromString s of
  Nothing -> error $ unwords ["Not a decimal literal in this test:", show s]
  Just dl -> dl
