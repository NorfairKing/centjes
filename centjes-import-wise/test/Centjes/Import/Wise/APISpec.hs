{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Centjes.Import.Wise.APISpec (spec) where

import Centjes.CurrencySymbol (CurrencySymbol (..))
import Centjes.Import.Wise.API
import Centjes.Import.Wise.Command.Fetch (statementFileName)
import Data.Time
import Path
import Test.Syd

spec :: Spec
spec = do
  describe "statementUrl" $ do
    -- Wise answers with whatever the last part of the path asks for, and the
    -- interval is a UTC instant rather than a day.  COMPACT is the type with
    -- one row per transaction, which is what this importer reads.
    it "asks for the CSV of one balance over an interval" $
      statementUrl
        "https://api.wise.com"
        12345
        (Balance 64 (CurrencySymbol "EUR"))
        (UTCTime (fromGregorian 2025 1 1) 0)
        (UTCTime (fromGregorian 2025 12 31) (86400 - 0.001))
        `shouldBe` "https://api.wise.com/v1/profiles/12345/balance-statements/64/statement.csv?currency=EUR&intervalStart=2025-01-01T00%3A00%3A00.000Z&intervalEnd=2025-12-31T23%3A59%3A59.999Z&type=COMPACT"

    it "asks the sandbox when that is the base URL" $
      statementUrl
        "https://api.sandbox.transferwise.tech"
        1
        (Balance 2 (CurrencySymbol "CHF"))
        (UTCTime (fromGregorian 2025 1 1) 0)
        (UTCTime (fromGregorian 2025 1 1) 0)
        `shouldSatisfy` startsWith "https://api.sandbox.transferwise.tech/v1/profiles/1/"

  describe "statementFileName" $ do
    it "names a statement after its profile and its currency" $
      statementFileName 12345 (Balance 64 (CurrencySymbol "EUR"))
        `shouldBe` Right [relfile|wise-12345-EUR.csv|]

    -- One token can reach a personal and a business profile, and both can hold
    -- euros.  Without the profile in the name one would overwrite the other and
    -- the import would silently lose half its history.
    it "gives two profiles' statements in one currency different names" $
      statementFileName 1 (Balance 64 (CurrencySymbol "EUR"))
        `shouldNotBe` statementFileName 2 (Balance 65 (CurrencySymbol "EUR"))

startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix
