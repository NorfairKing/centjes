{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Command.Balance (renderBalanceReport)
import Centjes.Compile
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Report.EvaluatedLedger
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
  describe "produceBalanceReportFromEvaluatedLedger" $ do
    it "produces valid reports" $
      forAllValid $ \f ->
        forAllValid $ \mY ->
          forAllValid $ \mCur ->
            forAllValid $ \showVirtual ->
              forAllValid $ \ledger ->
                case produceEvaluatedLedger @() ledger of
                  Failure _ -> pure () -- EvaluatedLedger may legitimately fail on generated ledgers
                  Success evaluatedLedger ->
                    shouldBeValid $ produceBalanceReportFromEvaluatedLedger @() f mY mCur showVirtual evaluatedLedger

    scenarioDirRecur "test_resources/balance/balanced" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "balances this module the same way" $
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
              Right BalanceSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds

                evaluatedLedger <-
                  shouldValidate diag $
                    produceEvaluatedLedger ledger

                br <-
                  shouldValidate diag $
                    produceBalanceReportFromEvaluatedLedger
                      balanceSettingFilter
                      balanceSettingEnd
                      balanceSettingCurrency
                      balanceSettingShowVirtual
                      evaluatedLedger

                shouldBeValid br
                pure $ renderChunksText termCaps $ renderBalanceReport balanceSettingShowEmpty br

    scenarioDir "test_resources/balance/error" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "balances this module the same way" $
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
              Right BalanceSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds

                -- Try producing the evaluated ledger first
                case produceEvaluatedLedger ledger of
                  Failure evaluatedLedgerErrors ->
                    pure $ renderValidationErrors diag evaluatedLedgerErrors
                  Success evaluatedLedger ->
                    -- Check assertions (account type and explicit assertions)
                    case checkEvaluatedLedgerAssertions evaluatedLedger of
                      Failure assertionErrors ->
                        pure $ renderValidationErrors diag assertionErrors
                      Success () -> do
                        errs <-
                          shouldFailToValidate $
                            produceBalanceReportFromEvaluatedLedger
                              balanceSettingFilter
                              balanceSettingEnd
                              balanceSettingCurrency
                              balanceSettingShowVirtual
                              evaluatedLedger

                        pure $ renderValidationErrors diag errs
