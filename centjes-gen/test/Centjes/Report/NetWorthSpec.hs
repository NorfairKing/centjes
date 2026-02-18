{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Report.NetWorthSpec (spec) where

import Centjes.Command.NetWorth (renderNetWorthReport)
import Centjes.Compile
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.NetWorth
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
import Text.Colour

spec :: Spec
spec = do
  describe "produceNetWorthReport" $ do
    scenarioDir "test_resources/net-worth/valid" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "produces the same net worth report" $
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
              Right NetWorthSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds

                report <-
                  shouldValidate diag $
                    produceNetWorthReport
                      netWorthSettingCurrency
                      netWorthSettingBegin
                      netWorthSettingEnd
                      ledger

                pure $ renderChunksText termCaps $ renderNetWorthReport report

    scenarioDir "test_resources/net-worth/error" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "produces the same net worth error" $
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
              Right NetWorthSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds
                errs <-
                  shouldFailToValidate $
                    produceNetWorthReport
                      netWorthSettingCurrency
                      netWorthSettingBegin
                      netWorthSettingEnd
                      ledger

                pure $ renderValidationErrors diag errs
