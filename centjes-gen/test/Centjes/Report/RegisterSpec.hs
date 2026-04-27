{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Report.RegisterSpec (spec) where

import Centjes.Block.Gen ()
import Centjes.Command.Register (renderAnyRegister)
import Centjes.Compile
import Centjes.Filter.Gen ()
import Centjes.Ledger.Gen ()
import Centjes.Load
import Centjes.Location (SourceSpan)
import Centjes.OptParse
import Centjes.Report.Register
import Centjes.Validation
import Centjes.Validation.TestUtils
import Control.Monad
import Control.Monad.Logger
import Data.Maybe (fromMaybe)
import Data.Ratio
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import qualified Money.Account as Account
import qualified Money.Account as Money (Account, Rounding (..))
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
  describe "produceBalanceReport" $ do
    xit "produces valid reports" $
      forAllValid $ \f ->
        forAllValid $ \bs ->
          forAllValid $ \mCurrencySymbolTo ->
            forAllValid $ \showVirtual ->
              forAllValid $ \mBegin ->
                forAllValid $ \mEnd ->
                  forAllValid $ \ledger ->
                    shouldBeValid $ produceRegister @() f bs mCurrencySymbolTo showVirtual mBegin mEnd ledger

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
                allCapabilities
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
                      registerSettingBlockSize
                      registerSettingCurrency
                      registerSettingShowVirtual
                      registerSettingBegin
                      registerSettingEnd
                      ledger

                shouldBeValid rr

                pure $ renderChunksText termCaps $ renderAnyRegister rr

    describe "register invariants" $ do
      scenarioDir "test_resources/register/valid" $ \fp -> do
        af <- liftIO $ resolveFile' fp
        when (fileExtension af == Just ".cent") $ do
          configFile <- liftIO $ replaceExtension ".config" af

          it "has running averages equal to runningTotal / blockIndex" $ do
            withSingleCurrencyRegister af configFile $ \blocks ->
              forM_ (zip [1 :: Integer ..] blocks) $ \(i, block) -> do
                let expected = expectedAverage (registerBlockRunningTotal block) i
                registerBlockRunningAverage block `shouldBe` expected

          it "has running totals consistent with cumulative block totals" $ do
            withSingleCurrencyRegister af configFile $ \blocks -> do
              let go _ [] = pure ()
                  go acc (block : rest) = do
                    let newAcc = fromMaybe acc (Account.add acc (registerBlockTotal block))
                    registerBlockRunningTotal block `shouldBe` newAcc
                    go newAcc rest
              go Account.zero blocks

          it "has block titles in strictly increasing order" $ do
            withSingleCurrencyRegister af configFile $ \blocks -> do
              let go [] = pure ()
                  go [_] = pure ()
                  go (b1 : b2 : rest) = do
                    registerBlockTitle b1 `shouldSatisfy` (< registerBlockTitle b2)
                    go (b2 : rest)
              go blocks

          it "has empty blocks with zero total and same running total as previous" $ do
            withSingleCurrencyRegister af configFile $ \blocks -> do
              let go _ [] = pure ()
                  go mPrev (block : rest) = do
                    when (V.null (registerBlockEntries block)) $ do
                      registerBlockTotal block `shouldBe` Account.zero
                      case mPrev of
                        Nothing -> pure ()
                        Just prev ->
                          registerBlockRunningTotal block
                            `shouldBe` registerBlockRunningTotal prev
                    go (Just block) rest
              go Nothing blocks

    scenarioDir "test_resources/register/error" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent") $ do
        resultFile <- liftIO $ replaceExtension ".err" af
        configFile <- liftIO $ replaceExtension ".config" af
        it "fails to produce a register for this module the same way" $
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
              Right RegisterSettings {..} -> do
                -- Load the module
                (ds, diag) <- runNoLoggingT $ loadModules af
                -- Compile to a ledger
                ledger <- shouldValidate diag $ compileDeclarations ds
                errs <-
                  shouldFailToValidate $
                    produceRegister
                      registerSettingFilter
                      registerSettingBlockSize
                      registerSettingCurrency
                      registerSettingShowVirtual
                      registerSettingBegin
                      registerSettingEnd
                      ledger

                pure $ renderValidationErrors diag errs

withSingleCurrencyRegister ::
  Path Abs File ->
  Path Abs File ->
  ([RegisterBlock 'SingleCurrency SourceSpan] -> IO ()) ->
  IO ()
withSingleCurrencyRegister af configFile assertion = do
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
    Right RegisterSettings {..} -> do
      (ds, diag) <- runNoLoggingT $ loadModules af
      ledger <- shouldValidate diag $ compileDeclarations ds
      rr <-
        shouldValidate diag $
          produceRegister
            registerSettingFilter
            registerSettingBlockSize
            registerSettingCurrency
            registerSettingShowVirtual
            registerSettingBegin
            registerSettingEnd
            ledger
      case rr of
        AnyConverted ConvertedRegister {..} ->
          assertion $ V.toList $ registerBlocks convertedRegister
        AnyMultiCurrency _ ->
          -- Multi-currency registers don't have single-currency blocks; skip
          pure ()

expectedAverage :: Money.Account -> Integer -> Money.Account
expectedAverage acc blockNum =
  let (ma, _) = Account.fraction Money.RoundDown acc (1 % blockNum)
   in fromMaybe Account.zero ma
