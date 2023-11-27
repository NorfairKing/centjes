{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Compile
import Centjes.DecimalLiteral
import Centjes.Format
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Report.Balance
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Scientific
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
  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      producesValid
        (produceBalanceReport @())
    tempDirSpec "centjes-balance-errors" $ do
      let usdSymbol = CurrencySymbol "USD"
      let usdDeclaration =
            DeclarationCurrency $
              noLoc $
                CurrencyDeclaration
                  { currencyDeclarationSymbol = noLoc usdSymbol,
                    currencyDeclarationQuantisationFactor = noLoc $ DecimalLiteral True 0 (scientific 1 (-2))
                  }

      it "shows the same error when an account's total amount balance gets too large" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_RUNNING_BALANCE.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 18446744073709551615 (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific (-18446744073709551615) (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        },
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 18446744073709551615 (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific (-18446744073709551615) (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        }
                ]
            }
      it "shows the same error when an account's total amount balance gets too large" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_ACCOUNT_TOTAL.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 18446744073709551615 (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 2 (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        }
                ]
            }

      it "shows the same error when the total transaction balance gets too large" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_TRANSACTION_SUM.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 18446744073709551615 (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 2 (-2))),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        }
                ]
            }

      it "shows the same error when a transaction is off-balance" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_OFF_BALANCE-simple.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 1 0)),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        }
                ]
            }
      it "shows the same TWO errors when a transaction is off-balance" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_OFF_BALANCE-two.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific 1 0)),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        },
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (Timestamp (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc (DecimalLiteral True 0 (scientific (-1) 0)),
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ]
                        }
                ]
            }

moduleGoldenBalanceError :: FilePath -> Module ann -> Path Abs Dir -> GoldenTest Text
moduleGoldenBalanceError file m tdir = do
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
    ledger <- shouldValidate $ compileDeclarations ds

    errs <- shouldFail $ produceBalanceReport ledger
    pure $ renderValidationErrors diag errs
