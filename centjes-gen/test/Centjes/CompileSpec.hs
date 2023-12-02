{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.CompileSpec (spec) where

import Centjes.Compile
import Centjes.DecimalLiteral
import Centjes.Format
import Centjes.Load
import Centjes.Location
import Centjes.Module
import qualified Centjes.Module as Module
import Centjes.Module.Gen ()
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.GenValidity.Map ()
import qualified Data.Map.Strict as M
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time
import Path
import Path.IO
import Test.QuickCheck (forAll)
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "compilePosting" $ do
    it "produces valid ledgers" $
      producesValid3 (compilePosting @())

    it "produces valid ledger postings if the currency is known" $
      forAllValid $ \posting ->
        forAllValid $ \factor ->
          let Located () symbol = Module.postingCurrencySymbol posting
           in case compilePosting (M.singleton symbol factor) () (Located () posting) of
                Failure _ -> pure ()
                Success p -> shouldBeValid p

  describe "compileTransaction" $ do
    it "produces valid ledgers" $
      producesValid2 (compileTransaction @())

    it "produces valid ledger transactions if all the currencies are known" $
      forAllValid $ \transaction ->
        forAll (sequence (M.fromSet (const genValid) (Module.transactionCurrencySymbols transaction))) $ \currencies -> do
          case compileTransaction currencies (Located () transaction) of
            Failure _ -> pure ()
            Success t -> shouldBeValid t

  describe "compileCurrency" $ do
    it "produces valid ledgers" $
      producesValid (compileCurrency @())

  describe "compileCurrencies" $ do
    it "produces valid ledgers" $
      producesValid (compileCurrencies @())

  describe "compileDeclarations" $ do
    it "produces valid ledgers" $
      producesValid (compileDeclarations @())

    tempDirSpec "centjes-compile-errors" $ do
      it "shows the same error when encountering an invalid quantisation factor" $
        moduleGoldenCompileError "test_resources/errors/compile/CE_INVALID_QUANTISATION_FACTOR.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ DeclarationCurrency $
                    noLoc $
                      CurrencyDeclaration
                        { currencyDeclarationSymbol = noLoc (CurrencySymbol "INVALID"),
                          currencyDeclarationQuantisationFactor = noLoc $ DecimalLiteral True 0 (scientific 1 2)
                        }
                ]
            }

      it "shows the same error when encountering a duplicate currency" $
        moduleGoldenCompileError "test_resources/errors/compile/CE_DUPLICATE_CURRENCY.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                let lSymbol = noLoc (CurrencySymbol "DUPLICATE")
                 in [ DeclarationCurrency $
                        noLoc $
                          CurrencyDeclaration
                            { currencyDeclarationSymbol = lSymbol,
                              currencyDeclarationQuantisationFactor = noLoc $ DecimalLiteral True 0 (scientific 1 (-2))
                            },
                      DeclarationCurrency $
                        noLoc $
                          CurrencyDeclaration
                            { currencyDeclarationSymbol = lSymbol,
                              currencyDeclarationQuantisationFactor = noLoc $ DecimalLiteral True 0 (scientific 5 (-2))
                            }
                    ]
            }
      it "shows the same error when encountering an undeclared currency" $
        moduleGoldenCompileError "test_resources/errors/compile/CE_UNDECLARED_CURRENCY.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 1 0)),
                                    postingCurrencySymbol = noLoc (CurrencySymbol "USD")
                                  }
                            ]
                        }
                ]
            }

      let usdSymbol = CurrencySymbol "USD"
      it "shows the same error when encountering an invalid amount" $
        moduleGoldenCompileError "test_resources/errors/compile/CE_INVALID_AMOUNT.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ DeclarationCurrency $
                    noLoc $
                      CurrencyDeclaration
                        { currencyDeclarationSymbol = noLoc usdSymbol,
                          currencyDeclarationQuantisationFactor = noLoc $ DecimalLiteral True 0 (scientific 1 (-2))
                        },
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 25)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 1 (-3))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        }
                ]
            }

moduleGoldenCompileError :: FilePath -> Module ann -> Path Abs Dir -> GoldenTest Text
moduleGoldenCompileError file m tdir = do
  goldenTextFile file $ do
    -- We have to write the module to a file to get the right source
    -- locations to produce a nice error.
    --
    -- Write the module to a file
    tfile <- resolveFile tdir "example-module.cent"
    SB.writeFile (fromAbsFile tfile) (TE.encodeUtf8 (formatModule m))

    -- Load the module
    (ds, diag) <- runNoLoggingT $ loadModules tfile
    -- Try to compil
    errs <- shouldFailToValidate $ compileDeclarations ds
    pure $ renderValidationErrors diag errs
