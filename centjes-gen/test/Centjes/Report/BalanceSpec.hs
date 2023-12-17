{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Compile
import Centjes.Format
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Report.Balance
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Path
import Path.IO
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "balanceTransaction" $ do
    it "produces valid balances" $
      producesValid $
        balanceTransaction @()

  let usdSymbol = CurrencySymbol "USD"
  let usdDeclaration =
        DeclarationCurrency $
          noLoc $
            CurrencyDeclaration
              { currencyDeclarationSymbol = noLoc usdSymbol,
                currencyDeclarationQuantisationFactor = noLoc "0.01"
              }
  let eurSymbol = CurrencySymbol "EUR"
  let eurDeclaration =
        DeclarationCurrency $
          noLoc $
            CurrencyDeclaration
              { currencyDeclarationSymbol = noLoc eurSymbol,
                currencyDeclarationQuantisationFactor = noLoc "0.01"
              }

  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      producesValid2
        (produceBalanceReport @())

    scenarioDir "test_resources/balance/balanced/as-is" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $
        it "balances this module" $ do
          -- Load the module
          (ds, diag) <- runNoLoggingT $ loadModules af
          -- Compile to a ledger
          ledger <- shouldValidate diag $ compileDeclarations ds

          br <- shouldValidate diag $ produceBalanceReport Nothing ledger
          shouldBeValid br

    scenarioDir "test_resources/balance/error/as-is" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        it "shows the same error when trying to balance this module" $ do
          goldenTextFile (fromAbsFile resultFile) $ do
            -- Load the module
            (ds, diag) <- runNoLoggingT $ loadModules af
            -- Compile to a ledger
            ledger <- shouldValidate diag $ compileDeclarations ds

            errs <- shouldFailToValidate $ produceBalanceReport Nothing ledger
            pure $ renderValidationErrors diag errs

    -- Can't turn these into golden tests yet because they use a currency conversion
    tempDirSpec "centjes-balance-errors" $ do
      it "shows the same error when the currency to convert to is not recognised" $
        moduleGoldenBalanceError' "test_resources/errors/balance-report/CONVERT_ERROR_UNKNOWN_TARGET.err" usdSymbol $
          Module [] []
      it "shows the same error when no price has been defined for a conversion" $
        moduleGoldenBalanceError' "test_resources/errors/balance-report/CONVERT_ERROR_MISSING_PRICE.err" eurSymbol $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  eurDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 12 15)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "1.0",
                                    postingCurrencySymbol = noLoc usdSymbol,
                                    postingCost = Nothing
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1.0",
                                    postingCurrencySymbol = noLoc usdSymbol,
                                    postingCost = Nothing
                                  }
                            ],
                          transactionExtras = []
                        }
                ]
            }
      it "shows the same error when the sum gets too large because of currency conversion" $
        moduleGoldenBalanceError' "test_resources/errors/balance-report/CONVERT_ERROR_INVALID_SUM.err" eurSymbol $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  eurDeclaration,
                  DeclarationPrice $
                    noLoc $
                      PriceDeclaration
                        { priceDeclarationTimestamp = noLoc (TimestampDay (fromGregorian 2023 12 16)),
                          priceDeclarationCurrencySymbol = noLoc usdSymbol,
                          priceDeclarationCost =
                            noLoc $
                              CostExpression
                                { costExpressionConversionRate = noLoc "200000000000000000",
                                  costExpressionCurrencySymbol = noLoc eurSymbol
                                }
                        },
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 12 15)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "1.0",
                                    postingCurrencySymbol = noLoc usdSymbol,
                                    postingCost = Nothing
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1.0",
                                    postingCurrencySymbol = noLoc usdSymbol,
                                    postingCost = Nothing
                                  }
                            ],
                          transactionExtras = []
                        }
                ]
            }

moduleGoldenBalanceError' :: FilePath -> CurrencySymbol -> Module ann -> Path Abs Dir -> GoldenTest Text
moduleGoldenBalanceError' fp cs = moduleGoldenBalanceErrorHelper fp $ Just cs

moduleGoldenBalanceErrorHelper :: FilePath -> Maybe CurrencySymbol -> Module ann -> Path Abs Dir -> GoldenTest Text
moduleGoldenBalanceErrorHelper file mCurrencyTo m tdir = do
  goldenTextFile file $ do
    -- We have to write the module to a file to get the right source
    -- locations to produce a nice error.
    --
    -- Write the module to a file
    tfile <- resolveFile tdir "example-module.cent"
    SB.writeFile (fromAbsFile tfile) (TE.encodeUtf8 (formatModule m))

    -- Load the module
    (ds, diag) <- runNoLoggingT $ loadModules tfile
    -- Compile to a ledger
    ledger <- shouldValidate diag $ compileDeclarations ds

    errs <- shouldFailToValidate $ produceBalanceReport mCurrencyTo ledger
    pure $ renderValidationErrors diag errs
