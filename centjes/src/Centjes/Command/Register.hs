{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Register
  ( runCentjesRegister,
    renderAnyRegister,
  )
where

import Centjes.Block
import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.OptParse
import Centjes.Report.Register
import Centjes.Timing
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Semigroup
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Money.Account as Account
import qualified Money.MultiAccount as Money (MultiAccount)
import Text.Colour
import Text.Colour.Layout

runCentjesRegister :: Settings -> RegisterSettings -> LoggingT IO ()
runCentjesRegister Settings {..} RegisterSettings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, fileMap) -> do
    let diagnostic = diagFromFileMap fileMap
    ledger <- withLoggedDuration "Compile" $ liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    register <-
      withLoggedDuration "Produce register" $
        liftIO $
          checkValidation diagnostic $
            produceRegister
              registerSettingFilter
              registerSettingBlockSize
              registerSettingCurrency
              registerSettingShowVirtual
              registerSettingBegin
              registerSettingEnd
              ledger
    liftIO $
      putChunksLocaleWith settingTerminalCapabilities $
        renderAnyRegister register

renderAnyRegister :: AnyRegister ann -> [Chunk]
renderAnyRegister = \case
  AnyMultiCurrency r -> renderMultiCurrencyRegister r
  AnyConverted cr -> renderConvertedRegister cr

renderMultiCurrencyRegister :: Register 'MultiCurrency ann -> [Chunk]
renderMultiCurrencyRegister register =
  let t = table (map (map pure) (renderMultiCurrencyRegisterTable register))
   in renderTable t

renderMultiCurrencyRegisterTable :: Register 'MultiCurrency ann -> [[Chunk]]
renderMultiCurrencyRegisterTable r@Register {..} =
  concat
    [ [ case registerBlockSize of
          BlockSizeIndividual ->
            [ "Timestamp",
              "Description",
              "Account",
              "Amount",
              " ",
              "Running",
              " "
            ]
          _ ->
            [ "Block",
              "Timestamp",
              "Description",
              "Account",
              "Amount",
              " ",
              "Running",
              " ",
              "Total",
              " ",
              "Average",
              " "
            ]
      ],
      let maxAccountWidth = registerMaxAccountWidthMulti r
       in concatMap
            (renderMultiBlock registerBlockSize maxAccountWidth)
            registerBlocks
    ]

renderMultiBlock ::
  BlockSize ->
  Max Word8 ->
  RegisterBlock 'MultiCurrency ann ->
  [[Chunk]]
renderMultiBlock blockSize maxAccountWidth RegisterBlock {..} =
  let blockLines =
        concatMap (renderMultiEntry maxAccountWidth) (V.toList registerBlockEntries)
      totalLines = multiAccountChunksWithWidth Nothing registerBlockRunningTotal
      averageLines = multiAccountChunksWithWidth Nothing registerBlockRunningAverage
      maxLines = max 1 (length blockLines)
   in case blockSize of
        BlockSizeIndividual -> blockLines
        _ ->
          hCatTable
            [ [[fore white $ chunk $ renderBlockTitle registerBlockTitle]],
              if V.null registerBlockEntries
                then [replicate 7 (chunk " ")]
                else blockLines,
              replicate (maxLines - length totalLines) [] ++ totalLines,
              replicate (maxLines - length averageLines) [] ++ averageLines
            ]

renderMultiEntry ::
  Max Word8 ->
  RegisterEntry 'MultiCurrency ann ->
  [[Chunk]]
renderMultiEntry maxAccountWidth (RegisterEntryTransaction RegisterTransaction {..}) =
  let Located _ ts = registerTransactionTimestamp
   in renderMultiTransaction
        maxAccountWidth
        ts
        (locatedValue <$> registerTransactionDescription)
        registerTransactionPostings

registerMaxAccountWidthMulti :: Register 'MultiCurrency ann -> Max Word8
registerMaxAccountWidthMulti Register {..} = foldMap goB registerBlocks
  where
    goB = foldMap goE . registerBlockEntries
    goE (RegisterEntryTransaction rt) = goT rt
    goT = foldMap goP . registerTransactionPostings
    goP RegisterPosting {..} =
      let Located _ Posting {..} = registerPosting
          Located _ a = postingAccount
       in accountWidth a <> multiAccountMaxWidth registerPostingBlockRunningTotal

renderMultiTransaction ::
  Max Word8 ->
  Timestamp ->
  Maybe Description ->
  Vector (RegisterPosting 'MultiCurrency ann) ->
  [[Chunk]]
renderMultiTransaction maxWidth timestamp mDescription postings =
  let postingLines =
        concatMap
          ( \RegisterPosting {..} ->
              let Located _ posting = registerPosting
               in renderMultiPosting maxWidth posting registerPostingBlockRunningTotal
          )
          $ V.toList postings
   in hCatTable
        [ [[timestampChunk timestamp]],
          maybe [] descriptionChunks mDescription,
          postingLines
        ]

renderMultiPosting ::
  Max Word8 ->
  Posting ann ->
  Money.MultiAccount (Currency ann) ->
  [[Chunk]]
renderMultiPosting maxWidth Posting {..} runningTotal =
  let Located _ accountName = postingAccountName
      Located _ Currency {..} = postingCurrency
      Located _ quantisationFactor = currencyQuantisationFactor
      Located _ acc = postingAccount
      f = fore $ if acc >= Account.zero then green else red
      postingChunks =
        [ accountNameChunk accountName,
          f $ accountChunkWithWidth (Just maxWidth) quantisationFactor acc,
          f $ currencySymbolChunk currencySymbol
        ]
      totalChunks = multiAccountChunksWithWidth (Just maxWidth) runningTotal
   in case totalChunks of
        [] -> [postingChunks ++ [chunk " ", chunk " "]]
        (cs : css) -> (postingChunks ++ cs) : map ([" ", " ", " "] ++) css

renderConvertedRegister :: ConvertedRegister ann -> [Chunk]
renderConvertedRegister ConvertedRegister {..} =
  let t = table (map (map pure) (renderConvertedRegisterTable convertedRegisterCurrency convertedRegister))
   in renderTable t

renderConvertedRegisterTable :: Currency ann -> Register 'SingleCurrency ann -> [[Chunk]]
renderConvertedRegisterTable currency r@Register {..} =
  concat
    [ [ case registerBlockSize of
          BlockSizeIndividual ->
            [ "Timestamp",
              "Description",
              "Account",
              "Amount",
              " ",
              "Running",
              " "
            ]
          _ ->
            [ "Block",
              "Timestamp",
              "Description",
              "Account",
              "Amount",
              " ",
              "Running",
              " ",
              "Total",
              " ",
              "Average",
              " "
            ]
      ],
      let maxAccountWidth = registerMaxAccountWidthSingle currency r
       in concatMap
            (renderSingleBlock registerBlockSize currency maxAccountWidth)
            registerBlocks
    ]

renderSingleBlock ::
  BlockSize ->
  Currency ann ->
  Max Word8 ->
  RegisterBlock 'SingleCurrency ann ->
  [[Chunk]]
renderSingleBlock blockSize currency maxAccountWidth RegisterBlock {..} =
  let blockLines =
        concatMap (renderSingleEntry currency maxAccountWidth) (V.toList registerBlockEntries)
      totalLines = singleAccountChunksWithWidth currency Nothing registerBlockRunningTotal
      averageLines = singleAccountChunksWithWidth currency Nothing registerBlockRunningAverage
      maxLines = max 1 (length blockLines)
   in case blockSize of
        BlockSizeIndividual -> blockLines
        _ ->
          hCatTable
            [ [[fore white $ chunk $ renderBlockTitle registerBlockTitle]],
              if V.null registerBlockEntries
                then [replicate 7 (chunk " ")]
                else blockLines,
              replicate (maxLines - length totalLines) [] ++ totalLines,
              replicate (maxLines - length averageLines) [] ++ averageLines
            ]

renderSingleEntry ::
  Currency ann ->
  Max Word8 ->
  RegisterEntry 'SingleCurrency ann ->
  [[Chunk]]
renderSingleEntry currency maxAccountWidth = \case
  RegisterEntryTransaction RegisterTransaction {..} ->
    let Located _ ts = registerTransactionTimestamp
     in renderSingleTransaction
          currency
          maxAccountWidth
          ts
          (locatedValue <$> registerTransactionDescription)
          registerTransactionPostings
  RegisterEntryRevaluation reval ->
    renderRevaluation currency maxAccountWidth reval

renderRevaluation ::
  Currency ann ->
  Max Word8 ->
  RegisterRevaluation ann ->
  [[Chunk]]
renderRevaluation currency maxWidth RegisterRevaluation {..} =
  let Located _ ts = registerRevaluationTimestamp
      Located _ cur = registerRevaluationCurrency
      amountChunks = singleAccountChunksWithWidth currency (Just maxWidth) registerRevaluationAmount
      runningChunks = singleAccountChunksWithWidth currency (Just maxWidth) registerRevaluationBlockRunningTotal
      renderLines = zipAmountAndRunning amountChunks runningChunks
      descriptionText = "Price: " <> currencySymbolText (currencySymbol cur)
   in hCatTable
        [ [[timestampChunk ts]],
          [[fore cyan $ chunk descriptionText]],
          renderLines
        ]
  where
    zipAmountAndRunning :: [[Chunk]] -> [[Chunk]] -> [[Chunk]]
    zipAmountAndRunning [] [] = [[chunk " ", chunk " ", chunk " ", chunk " ", chunk " "]]
    zipAmountAndRunning amts runs = go amts runs
      where
        go [] [] = []
        go (a : as) (r : rs) = ([chunk " "] ++ a ++ r) : go as rs
        go (a : as) [] = ([chunk " "] ++ a ++ [chunk " ", chunk " "]) : go as []
        go [] (r : rs) = ([chunk " ", chunk " ", chunk " "] ++ r) : go [] rs

registerMaxAccountWidthSingle :: Currency ann -> Register 'SingleCurrency ann -> Max Word8
registerMaxAccountWidthSingle currency Register {..} = foldMap goB registerBlocks
  where
    goB = foldMap goE . registerBlockEntries
    goE = \case
      RegisterEntryTransaction rt -> goT rt
      RegisterEntryRevaluation rr -> goR rr
    goT = foldMap goP . registerTransactionPostings
    goP RegisterPosting {..} =
      let Located _ Posting {..} = registerPosting
          Located _ a = postingAccount
       in accountWidth a <> singleAccountMaxWidth currency registerPostingBlockRunningTotal
    goR RegisterRevaluation {..} =
      singleAccountMaxWidth currency registerRevaluationAmount
        <> singleAccountMaxWidth currency registerRevaluationBlockRunningTotal

renderSingleTransaction ::
  Currency ann ->
  Max Word8 ->
  Timestamp ->
  Maybe Description ->
  Vector (RegisterPosting 'SingleCurrency ann) ->
  [[Chunk]]
renderSingleTransaction currency maxWidth timestamp mDescription postings =
  let postingLines =
        concatMap
          ( \RegisterPosting {..} ->
              let Located _ posting = registerPosting
               in renderSinglePosting currency maxWidth posting registerPostingBlockRunningTotal
          )
          $ V.toList postings
   in hCatTable
        [ [[timestampChunk timestamp]],
          maybe [] descriptionChunks mDescription,
          postingLines
        ]

renderSinglePosting ::
  Currency ann ->
  Max Word8 ->
  Posting ann ->
  Account.Account ->
  [[Chunk]]
renderSinglePosting currency maxWidth Posting {..} runningTotal =
  let Located _ accountName = postingAccountName
      Located _ Currency {..} = postingCurrency
      Located _ quantisationFactor = currencyQuantisationFactor
      Located _ acc = postingAccount
      f = fore $ if acc >= Account.zero then green else red
      postingChunks =
        [ accountNameChunk accountName,
          f $ accountChunkWithWidth (Just maxWidth) quantisationFactor acc,
          f $ currencySymbolChunk currencySymbol
        ]
      totalChunks = singleAccountChunksWithWidth currency (Just maxWidth) runningTotal
   in case totalChunks of
        [] -> [postingChunks ++ [chunk " ", chunk " "]]
        (cs : css) -> (postingChunks ++ cs) : map ([" ", " ", " "] ++) css

singleAccountChunksWithWidth :: Currency ann -> Maybe (Max Word8) -> Account.Account -> [[Chunk]]
singleAccountChunksWithWidth Currency {..} mMaxWidth acc =
  let Located _ quantisationFactor = currencyQuantisationFactor
      f = fore $ if acc >= Account.zero then green else red
   in [[f $ accountChunkWithWidth mMaxWidth quantisationFactor acc, f $ currencySymbolChunk currencySymbol]]

singleAccountMaxWidth :: Currency ann -> Account.Account -> Max Word8
singleAccountMaxWidth _ = accountWidth
