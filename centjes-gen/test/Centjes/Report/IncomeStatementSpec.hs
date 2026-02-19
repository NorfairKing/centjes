{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.IncomeStatementSpec (spec) where

import Centjes.Command.IncomeStatement (renderIncomeStatementReport)
import Centjes.Compile
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.EvaluatedLedger
import Centjes.Report.IncomeStatement
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified OptEnvConf
import qualified OptEnvConf.Args as Args
import OptEnvConf.Capability (allCapabilities)
import qualified OptEnvConf.EnvMap as EnvMap
import qualified OptEnvConf.Error as OptEnvConf
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
  describe "produceIncomeStatementReport" $ do
    it "produces valid reports" $
      forAllValid $ \f ->
        forAllValid $ \mBegin ->
          forAllValid $ \mEnd ->
            forAllValid $ \mCur ->
              forAllValid $ \showVirtual ->
                forAllValid $ \ledger ->
                  case produceEvaluatedLedger @() ledger of
                    Failure _ -> pure () -- EvaluatedLedger may legitimately fail on generated ledgers
                    Success evaluatedLedger ->
                      shouldBeValid $ produceIncomeStatementReport @() f mBegin mEnd mCur showVirtual evaluatedLedger

    scenarioDirRecur "test_resources/income-statement/valid" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "produces the expected income statement" $
          goldenTextFile (fromAbsFile reportFile) $ do
            mConfig <- do
              exists <- doesFileExist configFile
              if exists
                then Just <$> Yaml.decodeFileThrow (fromAbsFile configFile)
                else pure Nothing
            errOrSettings <-
              OptEnvConf.runParserOn
                allCapabilities
                Nothing
                OptEnvConf.settingsParser
                Args.emptyArgs
                EnvMap.empty
                mConfig
            let termCaps = With24BitColours
            case errOrSettings of
              Left errs -> expectationFailure $ T.unpack $ renderChunksText termCaps $ OptEnvConf.renderErrors errs
              Right IncomeStatementSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds

                evaluatedLedger <-
                  shouldValidate diag $
                    produceEvaluatedLedger ledger

                report <-
                  shouldValidate diag $
                    produceIncomeStatementReport
                      incomeStatementSettingFilter
                      incomeStatementSettingBegin
                      incomeStatementSettingEnd
                      incomeStatementSettingCurrency
                      incomeStatementSettingShowVirtual
                      evaluatedLedger

                shouldBeValid report
                pure $ renderChunksText termCaps $ renderIncomeStatementReport incomeStatementSettingShowEmpty report

    scenarioDir "test_resources/income-statement/error" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "produces the expected error" $
          goldenTextFile (fromAbsFile resultFile) $ do
            mConfig <- do
              exists <- doesFileExist configFile
              if exists
                then Just <$> Yaml.decodeFileThrow (fromAbsFile configFile)
                else pure Nothing
            errOrSettings <-
              OptEnvConf.runParserOn
                allCapabilities
                Nothing
                OptEnvConf.settingsParser
                Args.emptyArgs
                EnvMap.empty
                mConfig
            let termCaps = With24BitColours
            case errOrSettings of
              Left errs -> expectationFailure $ T.unpack $ renderChunksText termCaps $ OptEnvConf.renderErrors errs
              Right IncomeStatementSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds

                -- Try producing the evaluated ledger first
                case produceEvaluatedLedger ledger of
                  Failure evaluatedLedgerErrors ->
                    pure $ renderValidationErrors diag evaluatedLedgerErrors
                  Success evaluatedLedger -> do
                    errs <-
                      shouldFailToValidate $
                        produceIncomeStatementReport
                          incomeStatementSettingFilter
                          incomeStatementSettingBegin
                          incomeStatementSettingEnd
                          incomeStatementSettingCurrency
                          incomeStatementSettingShowVirtual
                          evaluatedLedger

                    pure $ renderValidationErrors diag errs
