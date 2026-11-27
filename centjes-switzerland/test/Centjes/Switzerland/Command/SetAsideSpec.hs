{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Centjes.Switzerland.Command.SetAsideSpec (spec) where

import Centjes.Compile
import Centjes.Load
import Centjes.Switzerland.Command.SetAside
import Centjes.Switzerland.Report.SetAside
import Centjes.Switzerland.TestUtils
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import qualified Data.List.NonEmpty as NE
import Data.Time
import OptEnvConf (runParser, settingsParser, withConfigurableYamlConfig)
import qualified OptEnvConf
import Path
import Path.IO
import Paths_centjes_switzerland
import System.Environment
import Test.Syd
import Text.Colour
import Text.Read (readMaybe)

spec :: Spec
spec = sequential $ do
  -- A new error does not compile without a kind, and a new kind fails the
  -- tests without a scenario.
  let setAsideErrorKind :: SetAsideError ann -> SetAsideErrorKind
      setAsideErrorKind = \case
        SetAsideErrorNoCHF -> SetAsideErrorKindNoCHF
        SetAsideErrorWrongCHF _ -> SetAsideErrorKindWrongCHF
        SetAsideErrorConvert _ -> SetAsideErrorKindConvert
        SetAsideErrorNoOfficialRateBefore _ _ -> SetAsideErrorKindNoOfficialRateBefore
        SetAsideErrorNoPrice _ _ -> SetAsideErrorKindNoPrice
        SetAsideErrorUndeclaredAccount _ -> SetAsideErrorKindUndeclaredAccount
        SetAsideErrorUntaggedWealth _ _ -> SetAsideErrorKindUntaggedWealth
        SetAsideErrorTaggedBoth _ _ _ -> SetAsideErrorKindTaggedBoth
        SetAsideErrorNoCantonalRates _ -> SetAsideErrorKindNoCantonalRates
        SetAsideErrorNoMunicipalSteuerfuss _ _ -> SetAsideErrorKindNoMunicipalSteuerfuss
        SetAsideErrorNoThirdPillarMaximum _ -> SetAsideErrorKindNoThirdPillarMaximum
        SetAsideErrorUndeclaredThirdPillarAccount _ -> SetAsideErrorKindUndeclaredThirdPillarAccount
        SetAsideErrorTaxes _ -> SetAsideErrorKindTaxes
        SetAsideErrorOverflow -> SetAsideErrorKindOverflow
  let errorScenario :: SetAsideErrorKind -> Path Rel Dir
      errorScenario = \case
        SetAsideErrorKindNoCHF -> [reldir|no-chf|]
        SetAsideErrorKindWrongCHF -> [reldir|wrong-chf|]
        SetAsideErrorKindConvert -> [reldir|convert|]
        SetAsideErrorKindNoOfficialRateBefore -> [reldir|no-official-rate-before|]
        SetAsideErrorKindNoPrice -> [reldir|no-price|]
        SetAsideErrorKindUndeclaredAccount -> [reldir|undeclared-account|]
        SetAsideErrorKindUntaggedWealth -> [reldir|untagged-wealth|]
        SetAsideErrorKindTaggedBoth -> [reldir|tagged-both|]
        SetAsideErrorKindNoCantonalRates -> [reldir|no-cantonal-rates|]
        SetAsideErrorKindNoMunicipalSteuerfuss -> [reldir|no-municipal-steuerfuss|]
        SetAsideErrorKindNoThirdPillarMaximum -> [reldir|no-third-pillar-maximum|]
        SetAsideErrorKindUndeclaredThirdPillarAccount -> [reldir|undeclared-third-pillar-account|]
        SetAsideErrorKindTaxes -> [reldir|taxes|]
        SetAsideErrorKindOverflow -> [reldir|overflow|]
  describe "renderSetAsideReport" $
    dirScenarioDir [reldir|test_set_aside|] $ \rd ->
      it "renders the same report for this scenario" $
        goldenTextFile (fromRelFile (rd </> [relfile|report.txt|])) $ do
          dir <- makeAbsolute rd
          withCurrentDir dir $ withArgs [] $ do
            (input, taxesInput) <-
              runParser version "centjes-switzerland set-aside-tests" $
                withConfigurableYamlConfig (OptEnvConf.runIO (resolveFile' "switzerland.yaml")) $
                  (,) <$> settingsParser <*> settingsParser
            ledgerFile <- resolveFile dir "ledger.cent"
            (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
            ledger <- shouldValidate diag $ compileDeclarations declarations
            pricesFile <- parseRelFile "currency-rates.cent"
            let today = fromGregorian 2026 10 30
            report <-
              shouldValidate diag $
                produceSetAsideReport input {setAsideInputYear = 2026} taxesInput today (splitPrices pricesFile ledger) ledger
            pure $ renderChunksText WithoutColours $ renderSetAsideReport report

  describe "produceSetAsideReport" $ do
    it "has a scenario for every kind of error, and no other scenarios" $ do
      errorsDir <- resolveDir' "test_set_aside_errors"
      (dirs, _) <- listDir errorsDir
      map dirname dirs
        `shouldMatchList` map errorScenario [minBound .. maxBound]

    forM_ [minBound .. maxBound] $ \kind -> do
      let rd = [reldir|test_set_aside_errors|] </> errorScenario kind
      it (unwords ["fails with only the", show kind, "error in its scenario"]) $
        goldenTextFile (fromRelFile (rd </> [relfile|error.txt|])) $ do
          dir <- makeAbsolute rd
          withCurrentDir dir $ withArgs [] $ do
            (input, taxesInput) <-
              runParser version "centjes-switzerland set-aside-tests" $
                withConfigurableYamlConfig (OptEnvConf.runIO (resolveFile' "switzerland.yaml")) $
                  (,) <$> settingsParser <*> settingsParser
            -- A scenario can be about another year than the default one.
            yearFile <- resolveFile dir "year"
            mYear <- forgivingAbsence $ readFile (fromAbsFile yearFile)
            year <- case mYear of
              Nothing -> pure 2026
              Just contents -> maybe (expectationFailure "the year file holds no year") pure $ readMaybe contents
            ledgerFile <- resolveFile dir "ledger.cent"
            (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
            ledger <- shouldValidate diag $ compileDeclarations declarations
            pricesFile <- parseRelFile "currency-rates.cent"
            let today = fromGregorian 2026 10 30
            case produceSetAsideReport input {setAsideInputYear = year} taxesInput today (splitPrices pricesFile ledger) ledger of
              ValidationSuccess _ -> expectationFailure "the report should have failed"
              ValidationFailure errs -> do
                map setAsideErrorKind (NE.toList errs) `shouldBe` [kind]
                pure $ renderValidationErrors diag errs

-- | Every kind of error that the report can fail with, each with a scenario
-- in its own directory.
data SetAsideErrorKind
  = SetAsideErrorKindNoCHF
  | SetAsideErrorKindWrongCHF
  | SetAsideErrorKindConvert
  | SetAsideErrorKindNoOfficialRateBefore
  | SetAsideErrorKindNoPrice
  | SetAsideErrorKindUndeclaredAccount
  | SetAsideErrorKindUntaggedWealth
  | SetAsideErrorKindTaggedBoth
  | SetAsideErrorKindNoCantonalRates
  | SetAsideErrorKindNoMunicipalSteuerfuss
  | SetAsideErrorKindNoThirdPillarMaximum
  | SetAsideErrorKindUndeclaredThirdPillarAccount
  | SetAsideErrorKindTaxes
  | SetAsideErrorKindOverflow
  deriving (Show, Eq, Enum, Bounded)
