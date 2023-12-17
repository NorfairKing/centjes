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
  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      producesValid2
        (produceBalanceReport @())
    tempDirSpec "centjes-balance-errors" $ do
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

      it "shows the same error when an account's total amount balance gets too large" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_RUNNING_BALANCE.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "184467440737095516.15",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-184467440737095516.15",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
                        },
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "184467440737095516.15",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-184467440737095516.15",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
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
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "184467440737095516.15",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "0.02",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
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
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "184467440737095516.15",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "0.02",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
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
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "1",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
                        }
                ]
            }

      it "shows the same error when a transaction with two postings is off-balance" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_OFF_BALANCE-two-postings.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "1",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1.5",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
                        }
                ]
            }

      it "does not show suggestions if postings have a different currency" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_OFF_BALANCE-two-currencies.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  eurDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "1",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1",
                                    postingCurrencySymbol = noLoc eurSymbol
                                  }
                            ],
                          transactionExtras = []
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
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "1",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
                        },
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 11 24)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
                        }
                ]
            }

      it "shows the same error when an assertion fails" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_ASSERTION.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 12 09)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "2",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-2",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras =
                            [ noLoc $
                                TransactionAssertion $
                                  noLoc $
                                    AssertionEquals
                                      (noLoc (AccountName "assets"))
                                      (noLoc "3")
                                      (noLoc usdSymbol)
                            ]
                        }
                ]
            }

      it "shows the same error when an assertion fails because of an incorrect currency" $
        moduleGoldenBalanceError "test_resources/errors/balance-report/BE_ASSERTION-wrong-currency.err" $
          Module
            { moduleImports = [],
              moduleDeclarations =
                [ usdDeclaration,
                  eurDeclaration,
                  DeclarationTransaction $
                    noLoc $
                      Transaction
                        { transactionTimestamp = noLoc (TimestampDay (fromGregorian 2023 12 09)),
                          transactionDescription = Nothing,
                          transactionPostings =
                            [ noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "assets"),
                                    postingAccount = noLoc "2",
                                    postingCurrencySymbol = noLoc eurSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-2",
                                    postingCurrencySymbol = noLoc eurSymbol
                                  }
                            ],
                          transactionExtras =
                            [ noLoc $
                                TransactionAssertion $
                                  noLoc $
                                    AssertionEquals
                                      (noLoc (AccountName "assets"))
                                      (noLoc "3")
                                      (noLoc usdSymbol)
                            ]
                        }
                ]
            }

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
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1.0",
                                    postingCurrencySymbol = noLoc usdSymbol
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
                                    postingCurrencySymbol = noLoc usdSymbol
                                  },
                              noLoc
                                Posting
                                  { postingAccountName = noLoc (AccountName "income"),
                                    postingAccount = noLoc "-1.0",
                                    postingCurrencySymbol = noLoc usdSymbol
                                  }
                            ],
                          transactionExtras = []
                        }
                ]
            }

moduleGoldenBalanceError :: FilePath -> Module ann -> Path Abs Dir -> GoldenTest Text
moduleGoldenBalanceError fp = moduleGoldenBalanceErrorHelper fp Nothing

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
    ledger <- shouldValidate $ compileDeclarations ds

    errs <- shouldFailToValidate $ produceBalanceReport mCurrencyTo ledger
    pure $ renderValidationErrors diag errs
