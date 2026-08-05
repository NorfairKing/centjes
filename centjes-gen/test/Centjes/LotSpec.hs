{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.LotSpec (spec) where

import Centjes.Compile
import Centjes.Ledger
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Location
import Centjes.Logging.TestUtils
import qualified Centjes.Module as Module
import Centjes.Module.Gen ()
import Centjes.Report.Check
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import qualified Data.Vector as V
import Path.IO
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "compilePosting" $
    it "converts a lot posting at the same rate as the same cost annotation" $
      forAllValid $ \currencies ->
        forAllValid $ \accounts ->
          forAllValid $ \posting ->
            forAllValid $ \costExpression -> do
              let compiled priceAnnotation =
                    compilePosting @()
                      currencies
                      accounts
                      ()
                      (Located () posting {Module.postingPrice = Just (Located () priceAnnotation)})
              case ( compiled (Module.PriceAnnotationCost costExpression),
                     compiled (Module.PriceAnnotationLot costExpression)
                   ) of
                (Success (Located _ costPosting), Success (Located _ lotPosting)) ->
                  case postingPrice lotPosting of
                    PostingPriceCurrency _ _ ->
                      expectationFailure "the lot annotation did not compile to a lot"
                    PostingPriceLot _ ->
                      (locatedValue <$> postingConversion lotPosting)
                        `shouldBe` (locatedValue <$> postingConversion costPosting)
                (Failure _, Failure _) -> pure ()
                (Success _, Failure _) ->
                  expectationFailure "the lot annotation failed to compile where the cost annotation did not"
                (Failure _, Success _) ->
                  expectationFailure "the cost annotation failed to compile where the lot annotation did not"

  describe "compileDeclarations" $ do
    it "makes an acquired lot its own commodity, worth what the underlying is worth" $ do
      af <- liftIO $ resolveFile' "test_resources/balance/balanced/lot-buy.cent"
      (ds, diag) <- runNoLoggingT $ loadModules af
      ledger <- shouldValidate diag $ compileDeclarations ds
      [ commodityText (locatedValue (postingCommodity posting))
        | Located _ transaction <- V.toList (ledgerTransactions ledger),
          Located _ posting <- V.toList (transactionPostings transaction)
        ]
        `shouldBe` ["EUR", "EUR", "SWDA lot @ 500 EUR", "EUR"]
      [ ( commodityText (locatedValue (priceCommodity price)),
          currencySymbolText (currencySymbol (locatedValue (costCurrency (locatedValue (priceCost price)))))
        )
        | Located _ price <- V.toList (ledgerPrices ledger)
        ]
        `shouldBe` [ ("SWDA lot @ 500 EUR", "SWDA"),
                     ("SWDA", "EUR")
                   ]

    it "emits no market price for a disposal" $ do
      af <- liftIO $ resolveFile' "test_resources/balance/balanced/lot-sale-gain.cent"
      (ds, diag) <- runNoLoggingT $ loadModules af
      ledger <- shouldValidate diag $ compileDeclarations ds
      [ ( commodityText (locatedValue (priceCommodity price)),
          currencySymbolText (currencySymbol (locatedValue (costCurrency (locatedValue (priceCost price)))))
        )
        | Located _ price <- V.toList (ledgerPrices ledger)
        ]
        `shouldBe` [ ("SWDA lot @ 500 EUR", "SWDA"),
                     ("SWDA", "EUR"),
                     ("SWDA lot @ 500 EUR", "SWDA")
                   ]

  describe "doCompleteCheck" $
    it "does not consider a currency that is only a lot basis unused" $ do
      af <- liftIO $ resolveFile' "test_resources/lot/basis-only.cent"
      (ds, diag) <- runTestLoggingT $ loadModules af
      void $ runTestLoggingT $ shouldValidateT diag $ doCompleteCheck ds
