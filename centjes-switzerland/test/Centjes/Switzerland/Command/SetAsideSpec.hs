{-# LANGUAGE LambdaCase #-}

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
  describe "renderSetAsideReport" $
    dirScenarioDir "test_set_aside" $ \fp ->
      it "renders the same report for this scenario" $
        goldenTextFile (fp <> "report.txt") $ do
          dir <- resolveDir' fp
          withCurrentDir dir $ withArgs [] $ do
            input <-
              runParser version "centjes-switzerland set-aside-tests" $
                withConfigurableYamlConfig (OptEnvConf.runIO (resolveFile' "switzerland.yaml")) settingsParser
            ledgerFile <- resolveFile dir "ledger.cent"
            (declarations, diag) <- runNoLoggingT $ loadModules ledgerFile
            ledger <- shouldValidate diag $ compileDeclarations declarations
            pricesFile <- parseRelFile "currency-rates.cent"
            let today = fromGregorian 2026 10 30
            report <-
              shouldValidate diag $
                produceSetAsideReport input {setAsideInputYear = 2026} today (splitPrices pricesFile ledger) ledger
            pure $ renderChunksText WithoutColours $ renderSetAsideReport report

  describe "produceSetAsideReport" $ do
    it "has a scenario for every kind of error, and no other scenarios" $ do
      errorsDir <- resolveDir' "test_set_aside_errors"
      (dirs, _) <- listDir errorsDir
      map (toFilePath . dirname) dirs
        `shouldMatchList` map (\kind -> errorScenario kind <> "/") [minBound .. maxBound]

    forM_ [minBound .. maxBound] $ \kind -> do
      let fp = "test_set_aside_errors/" <> errorScenario kind <> "/"
      it (unwords ["fails with only the", show kind, "error in its scenario"]) $
        goldenTextFile (fp <> "error.txt") $ do
          dir <- resolveDir' fp
          withCurrentDir dir $ withArgs [] $ do
            input <-
              runParser version "centjes-switzerland set-aside-tests" $
                withConfigurableYamlConfig (OptEnvConf.runIO (resolveFile' "switzerland.yaml")) settingsParser
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
            case produceSetAsideReport input {setAsideInputYear = year} today (splitPrices pricesFile ledger) ledger of
              Success _ -> expectationFailure "the report should have failed"
              Failure errs -> do
                map setAsideErrorKind (NE.toList errs) `shouldBe` [kind]
                pure $ renderValidationErrors diag errs

-- | Every kind of error that the report can fail with, each with a scenario
-- in its own directory.
data SetAsideErrorKind
  = KindNoCHF
  | KindWrongCHF
  | KindConvert
  | KindNoOfficialRateBefore
  | KindNoPrice
  | KindUndeclaredAccount
  | KindUntaggedWealth
  | KindTaggedBoth
  | KindNoCantonalRates
  | KindNoMunicipalSteuerfuss
  | KindOverflow
  deriving (Show, Eq, Enum, Bounded)

-- | A new error does not compile without a kind, and a new kind fails the
-- tests without a scenario.
setAsideErrorKind :: SetAsideError ann -> SetAsideErrorKind
setAsideErrorKind = \case
  SetAsideErrorNoCHF -> KindNoCHF
  SetAsideErrorWrongCHF _ -> KindWrongCHF
  SetAsideErrorConvert _ -> KindConvert
  SetAsideErrorNoOfficialRateBefore _ _ -> KindNoOfficialRateBefore
  SetAsideErrorNoPrice _ _ -> KindNoPrice
  SetAsideErrorUndeclaredAccount _ -> KindUndeclaredAccount
  SetAsideErrorUntaggedWealth _ _ -> KindUntaggedWealth
  SetAsideErrorTaggedBoth _ _ _ -> KindTaggedBoth
  SetAsideErrorNoCantonalRates _ -> KindNoCantonalRates
  SetAsideErrorNoMunicipalSteuerfuss _ _ -> KindNoMunicipalSteuerfuss
  SetAsideErrorOverflow -> KindOverflow

errorScenario :: SetAsideErrorKind -> FilePath
errorScenario = \case
  KindNoCHF -> "no-chf"
  KindWrongCHF -> "wrong-chf"
  KindConvert -> "convert"
  KindNoOfficialRateBefore -> "no-official-rate-before"
  KindNoPrice -> "no-price"
  KindUndeclaredAccount -> "undeclared-account"
  KindUntaggedWealth -> "untagged-wealth"
  KindTaggedBoth -> "tagged-both"
  KindNoCantonalRates -> "no-cantonal-rates"
  KindNoMunicipalSteuerfuss -> "no-municipal-steuerfuss"
  KindOverflow -> "overflow"
