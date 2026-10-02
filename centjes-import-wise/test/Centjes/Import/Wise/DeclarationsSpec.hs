{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Centjes.Import.Wise.DeclarationsSpec (spec) where

import Centjes.Format (formatModule, formatRationalExpression)
import Centjes.Import.Wise.Declarations
import Centjes.Import.Wise.Pair
import Centjes.Import.Wise.Statement
import Centjes.Location
import Centjes.Module
import Centjes.Parse (parseModule)
import Centjes.Parse.TestUtils (shouldParse)
import Centjes.Validation
import qualified Data.ByteString as SB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import qualified Money.Account as Account
import Money.QuantisationFactor (QuantisationFactor (..))
import qualified Numeric.DecimalLiteral as DecimalLiteral
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec = do
  describe "wiseTransactions" $ do
    it "writes the statements it was given" $
      goldenTextFile "test_resources/declarations/statements.cent" $
        render <$> importedTransactions

    it "writes one transaction per thing that happened" $ do
      transactions <- importedTransactions
      map descriptionOf transactions
        `shouldBe` [ Just "Received money from A Friend\nBaby gift",
                     Just "Card transaction of 450.85 EUR issued by Trainline\nTrainline",
                     Just "Received money from Boozt AB\nInvoice 2024-12",
                     Just "Converted 9559.05 EUR to 8983.40 CHF",
                     Just "Sent money to Neon\nTransfer to Neon"
                   ]

    -- A transaction in one currency has to add up to nothing on its own.  A
    -- conversion does not: its two sides are held together by the rate instead,
    -- which is what the conversionPrice tests below are about.
    it "writes single-currency transactions whose postings add up to nothing" $ do
      transactions <- importedTransactions
      let singleCurrency = filter ((== 1) . length . currenciesOf) transactions
      length singleCurrency `shouldBe` 4
      map (Account.sum . map amountOf . postingsOf) singleCurrency
        `shouldBe` map (const (Just Account.zero)) singleCurrency

    -- The statement says what the balance did; the postings have to say the
    -- same, or the assertion written after them claims something the
    -- transaction did not do.
    it "moves each balance by exactly what the statement says" $ do
      transactions <- importedTransactions
      rows <- importedRows
      assetsMovementPerCurrency transactions `shouldBe` statementMovementPerCurrency rows

    it "asserts the balance each statement row states" $ do
      transactions <- importedTransactions
      concatMap assertedBalances transactions
        `shouldBe` [ (CurrencySymbol "EUR", "+1406.00"),
                     (CurrencySymbol "EUR", "+955.15"),
                     (CurrencySymbol "EUR", "+9600.15"),
                     (CurrencySymbol "EUR", "+0.00"),
                     (CurrencySymbol "CHF", "+8983.40"),
                     (CurrencySymbol "CHF", "+0.00")
                   ]

    -- A fee is an expense that was charged, so the transaction it was charged
    -- on is the one the tags belong on.  Tagging everything would make the tags
    -- mean nothing.
    it "tags only what was charged a fee" $ do
      transactions <- importedTransactions
      map tagsOf transactions
        `shouldBe` [ [],
                     [],
                     [],
                     ["tax-deductible", "not-vat-deductible"],
                     ["tax-deductible", "not-vat-deductible"]
                   ]

    it "puts everything it writes back through the parser unchanged" $ do
      transactions <- importedTransactions
      here <- getCurrentDir
      let rendered = render transactions
      parsed <- shouldParse parseModule here [relfile|wise.cent|] rendered
      formatModule (stripModuleAnnotation parsed) `shouldBe` rendered

  describe "movementPostings" $ do
    -- A monthly card charge is a row that was nothing but a fee.  A posting of
    -- nothing to an account that had nothing to do with it would only be in the
    -- way.
    it "writes a row that was nothing but a fee without an empty other side" $
      fmap
        (map (\p -> (locatedValue (postingAccountName p), DecimalLiteral.toString (locatedValue (postingAccount p)))))
        (postingsOf <$> validated (oneTransaction (WiseMovement feeOnly)))
        `shouldBe` Just
          [ ("assets:wise", "-1.34"),
            ("expenses:banking:wise", "+1.34")
          ]

    it "still balances a row that was nothing but a fee" $
      fmap (Account.sum . map amountOf . postingsOf) (validated (oneTransaction (WiseMovement feeOnly)))
        `shouldBe` Just (Just Account.zero)

  describe "rowAmounts" $ do
    -- The amount column is the whole movement, fee and all.  Reading it as the
    -- amount before the fee would book the fee twice and leave the transaction
    -- short by it.
    it "takes the fee back out of what the balance moved by" $
      fmap
        (\amounts -> (rowAmountsMoved amounts, rowAmountsFee amounts, rowAmountsTransacted amounts))
        (validated (rowAmounts currencies conversionOut))
        `shouldBe` Just (account (-960015), account 4110, account (-955905))

    it "transacted what moved when nothing was charged" $
      fmap
        rowAmountsTransacted
        (validated (rowAmounts currencies conversionOut {rowTotalFees = Nothing}))
        `shouldBe` Just (account (-960015))

    it "cannot read a row in a currency the ledger does not declare" $
      validated (rowAmounts currencies conversionOut {rowCurrency = CurrencySymbol "XYZ"})
        `shouldBe` Nothing

  describe "conversionPrice" $ do
    -- Wise states a rate rounded to five decimals.  9559.05 EUR at 0.93977 is
    -- 8983.37 CHF, three centimes short of what actually arrived, so a
    -- transaction written with that rate would not balance.
    it "is the exact ratio of the two amounts, not the rate Wise rounded" $
      fmap renderPrice (conversionPrice (CurrencySymbol "CHF") (account (-955905)) (account 898340))
        `shouldBe` Just "898340 / 955905 CHF"

    it "has no rate for a conversion that converted nothing" $
      fmap renderPrice (conversionPrice (CurrencySymbol "CHF") Account.zero (account 1))
        `shouldBe` Nothing

    it "has no rate for a conversion that produced nothing" $
      fmap renderPrice (conversionPrice (CurrencySymbol "CHF") (account (-1)) Account.zero)
        `shouldBe` Nothing

  describe "eventDescription" $ do
    it "says what a conversion was when the statement says nothing" $
      fmap unDescription (eventDescription (WiseConversion (silent conversionOut) (silent conversionBack)))
        `shouldBe` Just "Convert EUR to CHF"

    it "says what the statement says when it says something" $
      fmap unDescription (eventDescription (WiseConversion conversionOut conversionBack))
        `shouldBe` Just "Converted 9559.05 EUR to 8983.40 CHF"

    -- A description is made of non-empty lines, and a statement cell can hold a
    -- newline, so a description that kept one could not be read back.
    it "keeps a description that spans lines on one line" $
      fmap unDescription (eventDescription (WiseMovement (silent conversionOut) {rowDescription = "one\ntwo"}))
        `shouldBe` Just "one two"

    it "says what a row says once, however many columns say it" $
      fmap
        unDescription
        ( eventDescription
            (WiseMovement (silent conversionOut) {rowDescription = "Trainline", rowMerchant = "Trainline"})
        )
        `shouldBe` Just "Trainline"

importedRows :: IO [Row]
importedRows = do
  let readStatement fp = do
        contents <- SB.readFile fp
        case decodeStatement contents of
          Left err -> expectationFailure $ unlines [unwords ["Could not read", fp], err]
          Right rows -> pure rows
  eur <- readStatement "test_resources/statements/EUR.csv"
  chf <- readStatement "test_resources/statements/CHF.csv"
  pure (eur ++ chf)

importedTransactions :: IO [Transaction ()]
importedTransactions = do
  rows <- importedRows
  events <- case pairRows rows of
    Left errs -> expectationFailure $ unlines (map renderPairError errs)
    Right events -> pure events
  case wiseTransactions testSettings currencies events of
    Failure errs ->
      expectationFailure $
        unwords ["Could not write these statements, with", show (length errs), "errors"]
    Success transactions -> pure transactions

testSettings :: DeclarationSettings
testSettings =
  DeclarationSettings
    { declarationSettingAssetsAccountName = "assets:wise",
      declarationSettingExpensesAccountName = "expenses:unknown:wise",
      declarationSettingIncomeAccountName = "income:unknown:wise",
      declarationSettingFeesAccountName = "expenses:banking:wise",
      declarationSettingFeeTags = ["tax-deductible", "not-vat-deductible"]
    }

currencies :: Map CurrencySymbol (GenLocated () QuantisationFactor)
currencies =
  M.fromList
    [ (CurrencySymbol "EUR", noLoc centimes),
      (CurrencySymbol "CHF", noLoc centimes)
    ]

centimes :: QuantisationFactor
centimes = QuantisationFactor 100

render :: [Transaction ()] -> Text
render transactions =
  formatModule
    Module
      { moduleImports = [],
        moduleDeclarations = map (noLoc . DeclarationTransaction . noLoc) transactions
      }

descriptionOf :: Transaction () -> Maybe Text
descriptionOf t = unDescription . locatedValue . commentedValue <$> transactionDescription t

postingsOf :: Transaction () -> [Posting ()]
postingsOf t = [p | Commented (Located _ p) _ <- transactionPostings t]

currenciesOf :: Transaction () -> [CurrencySymbol]
currenciesOf t = M.keys (M.fromList [(locatedValue (postingCurrencySymbol p), ()) | p <- postingsOf t])

amountOf :: Posting () -> Account.Account
amountOf = accountOf . locatedValue . postingAccount

accountOf :: DecimalLiteral -> Account.Account
accountOf literal = case Account.fromDecimalLiteral centimes literal of
  Nothing -> error $ unwords ["Not a centime-sized amount in this test:", show literal]
  Just a -> a

account :: Integer -> Account.Account
account i = case Account.fromMinimalQuantisations i of
  Nothing -> error $ unwords ["Not a valid account in this test:", show i]
  Just a -> a

oneTransaction :: WiseEvent -> Validation ImportError (Transaction ())
oneTransaction event = case wiseTransactions testSettings currencies [event] of
  Failure errs -> Failure errs
  Success [t] -> Success t
  Success transactions ->
    error $ unwords ["Expected one transaction in this test, got", show (length transactions)]

-- | A row that was nothing but a fee, such as a monthly card charge.
feeOnly :: Row
feeOnly =
  conversionOut
    { rowId = "FEE-1",
      rowAmount = literalOf "-1.34",
      rowRunningBalance = Nothing,
      rowExchangeFrom = Nothing,
      rowExchangeTo = Nothing,
      rowTotalFees = Just (literalOf "1.34")
    }

validated :: Validation e a -> Maybe a
validated = \case
  Failure _ -> Nothing
  Success a -> Just a

sumAccounts :: [Account.Account] -> Account.Account
sumAccounts accounts = case Account.sum accounts of
  Nothing -> error "The accounts in this test do not add up."
  Just total -> total

-- | What every transaction together did to the assets account, per currency.
assetsMovementPerCurrency :: [Transaction ()] -> Map CurrencySymbol Account.Account
assetsMovementPerCurrency transactions =
  M.map sumAccounts $
    M.fromListWith
      (++)
      [ (locatedValue (postingCurrencySymbol p), [amountOf p])
      | t <- transactions,
        p <- postingsOf t,
        locatedValue (postingAccountName p) == "assets:wise"
      ]

-- | What the statements say each balance did.
statementMovementPerCurrency :: [Row] -> Map CurrencySymbol Account.Account
statementMovementPerCurrency rows =
  M.map sumAccounts $
    M.fromListWith (++) [(rowCurrency r, [accountOf (rowAmount r)]) | r <- rows]

-- | Every balance a transaction claims, with the currency it claims it in.
assertedBalances :: Transaction () -> [(CurrencySymbol, String)]
assertedBalances t =
  [ (currencyOfCommodity commodity, DecimalLiteral.toString literal)
  | Commented (Located _ (TransactionAssertion (Located _ (ExtraAssertion (Located _ assertion))))) _ <-
      transactionExtras t,
    let AssertionEquals _ _ (Located _ literal) (Located _ commodity) = assertion
  ]

currencyOfCommodity :: CommodityExpression () -> CurrencySymbol
currencyOfCommodity = \case
  CommodityExpressionCurrency (Located _ cs) -> cs
  CommodityExpressionLot (Located _ cs) _ -> cs

tagsOf :: Transaction () -> [Tag]
tagsOf t =
  [ locatedValue lt
  | Commented (Located _ (TransactionTag (Located _ (ExtraTag lt)))) _ <- transactionExtras t
  ]

renderPrice :: PriceAnnotation () -> Text
renderPrice priceAnnotation =
  let Located _ costExpression = priceAnnotationCostExpression priceAnnotation
      Located _ rationalExpression = costExpressionConversionRate costExpression
      Located _ currency = costExpressionCurrencySymbol costExpression
   in mconcat
        [ formatRationalExpression rationalExpression,
          " ",
          currencySymbolText currency
        ]

-- | A row with nothing to say about itself, so that what is written down comes
-- from what the row did rather than from what it said.
silent :: Row -> Row
silent row = row {rowDescription = "", rowMerchant = "", rowPaymentReference = ""}

conversionOut :: Row
conversionOut =
  Row
    { rowId = "CONVERSION-1",
      rowDate = fromGregorian 2025 1 15,
      rowAmount = literalOf "-9600.15",
      rowCurrency = CurrencySymbol "EUR",
      rowDescription = "Converted 9559.05 EUR to 8983.40 CHF",
      rowPaymentReference = "",
      rowMerchant = "",
      rowRunningBalance = Just (literalOf "0.00"),
      rowExchangeFrom = Just (CurrencySymbol "EUR"),
      rowExchangeTo = Just (CurrencySymbol "CHF"),
      rowTotalFees = Just (literalOf "41.10")
    }

conversionBack :: Row
conversionBack =
  conversionOut
    { rowAmount = literalOf "8983.40",
      rowCurrency = CurrencySymbol "CHF",
      rowRunningBalance = Just (literalOf "8983.40"),
      rowTotalFees = Just (literalOf "0.00")
    }

literalOf :: String -> DecimalLiteral
literalOf s = case DecimalLiteral.fromString s of
  Nothing -> error $ unwords ["Not a decimal literal in this test:", show s]
  Just dl -> dl
