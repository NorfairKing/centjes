{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.RegisterSpec (spec) where

import Centjes.Command.Register (renderRegister)
import Centjes.Compile
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.OptParse
import Centjes.Report.Register
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
  describe "produceBalanceReport" $ do
    it "produces valid reports" $
      forAllValid $ \f ->
        forAllValid $ \mCurrencySymbolTo ->
          forAllValid $ \showVirtual ->
            forAllValid $ \mBegin ->
              forAllValid $ \mEnd ->
                forAllValid $ \ledger ->
                  shouldBeValid $ produceRegister @() f mCurrencySymbolTo showVirtual mBegin mEnd ledger

    scenarioDir "test_resources/register/valid" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        reportFile <- liftIO $ replaceExtension ".txt" af
        configFile <- liftIO $ replaceExtension ".config" af

        it "renders a register for this module the same way" $ do
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
              Right RegisterSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds

                rr <-
                  shouldValidate diag $
                    produceRegister
                      registerSettingFilter
                      registerSettingCurrency
                      registerSettingShowVirtual
                      registerSettingBegin
                      registerSettingEnd
                      ledger

                shouldBeValid rr

                pure $ renderChunksText termCaps $ renderRegister rr

-- TODO error tests too.
