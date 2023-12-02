{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Command.CheckSpec (spec) where

import Centjes.Command.Check
import Centjes.DecimalLiteral
import Centjes.Format
import Centjes.Load
import Centjes.Location
import Centjes.Module
import Centjes.Module.Gen ()
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
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
  describe "checkAccountsUnique" $
    it "finds duplicate accounts" $
      forAllValid $ \an -> do
        let ad1 = AccountDeclaration {accountDeclarationName = noLoc an}
        let ad2 = AccountDeclaration {accountDeclarationName = noLoc an}
        errs <- shouldFailToValidate $ checkAccountsUnique [DeclarationAccount $ noLoc ad1, DeclarationAccount $ noLoc ad2]
        errs `shouldBe` CheckErrorAccountDeclaredTwice () () an :| []

  describe "checkAccountsDeclared" $
    it "finds undeclared accounts" $
      forAllValid $ \t' ->
        forAllValid $ \p -> do
          let t :: GenLocated () (Transaction ())
              t = noLoc $ t' {transactionPostings = [noLoc p]}
              Located _ an = postingAccountName p
          let td = DeclarationTransaction t
          errs <- shouldFailToValidate $ checkAccountsDeclared M.empty [td]
          errs `shouldBe` CheckErrorUndeclaredAccount () () (Located () an) :| []

  describe "checkDeclarations" $
    it "produces valid errors" $
      producesValid (checkDeclarations @())

  describe "doCompleteCheck" $ do
    it "produces valid errors" $
      producesValid (doCompleteCheck @())
    tempDirSpec "centjes-check-errors" $ do
      it "shows the same error when encountering a duplicate account definition" $
        moduleGoldenCheckError "test_resources/errors/check/CE_DUPLICATE_ACCOUNT.err" $
          let ad = DeclarationAccount $ noLoc $ AccountDeclaration {accountDeclarationName = noLoc (AccountName "duplicate")}
           in Module
                { moduleImports = [],
                  moduleDeclarations =
                    [ ad,
                      ad
                    ]
                }
      it "shows the same error when encountering undeclared account" $
        moduleGoldenCheckError "test_resources/errors/check/CE_UNDECLARED_ACCOUNT.err" $
          let usdSymbol = CurrencySymbol "USD"
              usdDeclaration =
                DeclarationCurrency $
                  noLoc $
                    CurrencyDeclaration
                      { currencyDeclarationSymbol = noLoc usdSymbol,
                        currencyDeclarationQuantisationFactor = noLoc $ DecimalLiteral True 0 (scientific 1 (-2))
                      }
           in Module
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
                                      { postingAccountName = noLoc (AccountName "undeclared"),
                                        postingAccount = noLoc (DecimalLiteral True 0 (scientific 1 0)),
                                        postingCurrencySymbol = noLoc usdSymbol
                                      }
                                ]
                            }
                    ]
                }

moduleGoldenCheckError :: FilePath -> Module ann -> Path Abs Dir -> GoldenTest Text
moduleGoldenCheckError file m tdir = do
  goldenTextFile file $ do
    -- We have to write the module to a file to get the right source
    -- locations to produce a nice error.
    --
    -- Write the module to a file
    tfile <- resolveFile tdir "example-module.cent"
    SB.writeFile (fromAbsFile tfile) (TE.encodeUtf8 (formatModule m))

    -- Load the module
    (ds, diag) <- runNoLoggingT $ loadModules tfile
    -- Try to check
    errs <- shouldFailToValidate $ doCompleteCheck ds
    pure $ renderValidationErrors diag errs
