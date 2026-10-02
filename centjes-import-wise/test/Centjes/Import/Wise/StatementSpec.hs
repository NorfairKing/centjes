{-# LANGUAGE OverloadedStrings #-}

module Centjes.Import.Wise.StatementSpec (spec) where

import Centjes.CurrencySymbol (CurrencySymbol (..))
import Centjes.Import.Wise.Statement
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.Either (isLeft)
import Data.Time.Calendar (fromGregorian)
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Test.Syd

spec :: Spec
spec = do
  describe "decodeStatement" $ do
    it "reads a statement with a payment, a card transaction and a conversion" $ do
      contents <- SB.readFile "test_resources/statements/EUR.csv"
      decodeStatement contents
        `shouldBe` Right
          [ Row
              { rowId = "TRANSFER-1",
                rowDate = fromGregorian 2025 1 3,
                rowAmount = literal "50.00",
                rowCurrency = CurrencySymbol "EUR",
                rowDescription = "Received money from A Friend",
                rowPaymentReference = "Baby gift",
                rowMerchant = "",
                rowRunningBalance = Just (literal "1406.00"),
                rowExchangeFrom = Nothing,
                rowExchangeTo = Nothing,
                rowTotalFees = Just (literal "0.00")
              },
            Row
              { rowId = "CARD-1",
                rowDate = fromGregorian 2025 1 8,
                rowAmount = literal "-450.85",
                rowCurrency = CurrencySymbol "EUR",
                rowDescription = "Card transaction of 450.85 EUR issued by Trainline",
                rowPaymentReference = "",
                rowMerchant = "Trainline",
                rowRunningBalance = Just (literal "955.15"),
                rowExchangeFrom = Nothing,
                rowExchangeTo = Nothing,
                rowTotalFees = Just (literal "0.00")
              },
            Row
              { rowId = "TRANSFER-2",
                rowDate = fromGregorian 2025 1 15,
                rowAmount = literal "8645.00",
                rowCurrency = CurrencySymbol "EUR",
                rowDescription = "Received money from Boozt AB",
                rowPaymentReference = "Invoice 2024-12",
                rowMerchant = "",
                rowRunningBalance = Just (literal "9600.15"),
                rowExchangeFrom = Nothing,
                rowExchangeTo = Nothing,
                rowTotalFees = Just (literal "0.00")
              },
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
          ]

    it "reads the other side of that conversion and a transfer out" $ do
      contents <- SB.readFile "test_resources/statements/CHF.csv"
      decodeStatement contents
        `shouldBe` Right
          [ Row
              { rowId = "CONVERSION-1",
                rowDate = fromGregorian 2025 1 15,
                rowAmount = literal "8983.40",
                rowCurrency = CurrencySymbol "CHF",
                rowDescription = "Converted 9559.05 EUR to 8983.40 CHF",
                rowPaymentReference = "",
                rowMerchant = "",
                rowRunningBalance = Just (literal "8983.40"),
                rowExchangeFrom = Just (CurrencySymbol "EUR"),
                rowExchangeTo = Just (CurrencySymbol "CHF"),
                rowTotalFees = Just (literal "0.00")
              },
            Row
              { rowId = "TRANSFER-3",
                rowDate = fromGregorian 2025 1 16,
                rowAmount = literal "-8983.40",
                rowCurrency = CurrencySymbol "CHF",
                rowDescription = "Sent money to Neon",
                rowPaymentReference = "Transfer to Neon",
                rowMerchant = "",
                rowRunningBalance = Just (literal "0.00"),
                rowExchangeFrom = Nothing,
                rowExchangeTo = Nothing,
                rowTotalFees = Just (literal "1.34")
              }
          ]

    -- Wise writes a statement date day first.  Read the other way around, the
    -- third of January becomes the first of March, which is the kind of mistake
    -- that only turns up at the end of a quarter.
    it "reads a date day first" $
      fmap (map rowDate) (decodeStatement (header <> "\nX,03-01-2025,1.00,EUR,A payment,,1.00,,,,,,,,,,,,0.00,\n"))
        `shouldBe` Right [fromGregorian 2025 1 3]

    it "reads an ISO date too" $
      fmap (map rowDate) (decodeStatement (header <> "\nX,2025-01-03,1.00,EUR,A payment,,1.00,,,,,,,,,,,,0.00,\n"))
        `shouldBe` Right [fromGregorian 2025 1 3]

    -- Wise has changed which columns a statement carries.  A statement missing
    -- a column this importer only reads when it is there says the same thing as
    -- one whose cell is empty.
    it "reads a statement that has no fee or exchange columns at all" $
      decodeStatement "TransferWise ID,Date,Amount,Currency,Description,Running Balance\nX,03-01-2025,1.00,EUR,A payment,1.00\n"
        `shouldBe` Right
          [ Row
              { rowId = "X",
                rowDate = fromGregorian 2025 1 3,
                rowAmount = literal "1.00",
                rowCurrency = CurrencySymbol "EUR",
                rowDescription = "A payment",
                rowPaymentReference = "",
                rowMerchant = "",
                rowRunningBalance = Just (literal "1.00"),
                rowExchangeFrom = Nothing,
                rowExchangeTo = Nothing,
                rowTotalFees = Nothing
              }
          ]

    it "refuses a statement without the columns that say what a row is" $
      decodeStatement "Date,Amount\n03-01-2025,1.00\n" `shouldSatisfy` isLeft

  describe "rowIsConversionLeg" $ do
    it "says a row that exchanged one currency for another is one" $
      rowIsConversionLeg (exampleRow {rowExchangeFrom = Just (CurrencySymbol "EUR"), rowExchangeTo = Just (CurrencySymbol "CHF")})
        `shouldBe` True

    it "says an ordinary payment is not one" $
      rowIsConversionLeg exampleRow `shouldBe` False

    -- Wise fills these in on a transfer that did not change currency as well,
    -- and that has no second side to pair with.
    it "says a row that exchanged a currency for itself is not one" $
      rowIsConversionLeg (exampleRow {rowExchangeFrom = Just (CurrencySymbol "EUR"), rowExchangeTo = Just (CurrencySymbol "EUR")})
        `shouldBe` False

    it "says a row that only names one side is not one" $
      rowIsConversionLeg (exampleRow {rowExchangeFrom = Just (CurrencySymbol "EUR")}) `shouldBe` False

header :: ByteString
header = "TransferWise ID,Date,Amount,Currency,Description,Payment Reference,Running Balance,Exchange From,Exchange To,Exchange Rate,Payer Name,Payee Name,Payee Account Number,Merchant,Card Last Four Digits,Card Holder Full Name,Attachment,Note,Total fees,Exchange To Amount"

exampleRow :: Row
exampleRow =
  Row
    { rowId = "X",
      rowDate = fromGregorian 2025 1 3,
      rowAmount = literal "1.00",
      rowCurrency = CurrencySymbol "EUR",
      rowDescription = "A payment",
      rowPaymentReference = "",
      rowMerchant = "",
      rowRunningBalance = Nothing,
      rowExchangeFrom = Nothing,
      rowExchangeTo = Nothing,
      rowTotalFees = Nothing
    }

literal :: String -> DecimalLiteral.DecimalLiteral
literal s = case DecimalLiteral.fromString s of
  Nothing -> error $ unwords ["Not a decimal literal in this test:", show s]
  Just dl -> dl
