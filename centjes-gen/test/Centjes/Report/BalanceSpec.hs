{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.BalanceSpec (spec) where

import Centjes.Command.Balance (renderBalanceReportTable)
import Centjes.Compile
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Balance
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified OptEnvConf
import qualified OptEnvConf.Args as Args
import qualified OptEnvConf.EnvMap as EnvMap
import qualified OptEnvConf.Error as OptEnvConf
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec = do
  describe "balanceTransaction" $ do
    it "produces valid balances" $
      producesValid2 $
        balanceTransaction @()

  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      forAllValid $ \f ->
        forAllValid $ \mCur ->
          forAllValid $ \showVirtual ->
            forAllValid $ \ledger ->
              shouldBeValid $ produceBalanceReport @() f mCur showVirtual ledger

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

                br <-
                  shouldValidate diag $
                    produceBalanceReport
                      balanceSettingFilter
                      balanceSettingCurrency
                      balanceSettingShowVirtual
                      ledger

                shouldBeValid br
                pure $ renderChunksText termCaps $ renderBalanceReportTable ShowEmpty br

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
                errs <-
                  shouldFailToValidate $
                    produceBalanceReport
                      balanceSettingFilter
                      balanceSettingCurrency
                      balanceSettingShowVirtual
                      ledger

                pure $ renderValidationErrors diag errs
