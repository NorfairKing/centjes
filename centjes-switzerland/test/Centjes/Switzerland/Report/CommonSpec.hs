{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Report.CommonSpec (spec) where

import Centjes.Ledger
import Centjes.Ledger.Gen ()
import Centjes.Location
import Centjes.Switzerland.Report.Common
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Money.Account (Account (..))
import qualified Money.Account as Account
import qualified Money.Amount as Amount
import qualified Money.ConversionRate as ConversionRate
import qualified Money.MultiAccount as MultiAccount
import Money.MultiAccount.Gen ()
import Money.QuantisationFactor (QuantisationFactor (..))
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec =
  describe "combineLots" $ do
    it "produces valid multi-accounts" $
      producesValid (combineLots @())

    -- Note: not stated as "the grand total is preserved", because Account.sum
    -- folds left and the combined entries are larger, so it can overflow where
    -- the same amounts spread over more entries do not.
    --
    -- A currency whose commodities cancel out is absent rather than zero, the
    -- way MultiAccount represents a zero balance everywhere else. Random
    -- amounts never cancel, so the fixed case below is what covers that.
    it "gives each currency the sum of the commodities that share it" $
      forAllValid $ \ma ->
        case combineLots @() ma of
          Nothing -> pure ()
          Just combined ->
            forM_ (S.toList (S.map commodityCurrency (M.keysSet (MultiAccount.unMultiAccount ma)))) $ \currency ->
              case Account.sum
                [ a
                | (commodity, a) <- M.toList (MultiAccount.unMultiAccount ma),
                  commodityCurrency commodity == currency
                ] of
                Nothing -> expectationFailure "a group sum overflowed even though combineLots succeeded"
                Just s ->
                  M.lookup currency (MultiAccount.unMultiAccount combined)
                    `shouldBe` if s == Account.zero then Nothing else Just s

    it "drops a currency whose lots cancel out" $ do
      let eur = Currency (CurrencySymbol "EUR") (Located () (QuantisationFactor 100))
      let chf = Currency (CurrencySymbol "CHF") (Located () (QuantisationFactor 100))
      let swda = Currency (CurrencySymbol "SWDA") (Located () (QuantisationFactor 1))
      let five = Positive (Amount.Amount 5)
      let minusFive = Negative (Amount.Amount 5)
      MultiAccount.unMultiAccount
        <$> combineLots
          ( MultiAccount.MultiAccount
              ( M.fromList
                  [ (CommodityLot (Lot swda ConversionRate.oneToOne eur), five),
                    (CommodityLot (Lot swda ConversionRate.oneToOne chf), minusFive)
                  ]
              )
          )
        `shouldBe` Just M.empty
