{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Register
  ( runCentjesRegister,
    renderRegister,
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
        renderRegister register

renderRegister :: Register ann -> [Chunk]
renderRegister register =
  let t = table (map (map pure) (renderRegisterTable register))
   in renderTable t

renderRegisterTable :: Register ann -> [[Chunk]]
renderRegisterTable r@Register {..} =
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
      let maxAccountWidth = registerMaxAccountWidth r
       in concatMap
            (renderBlock registerBlockSize maxAccountWidth)
            registerBlocks
    ]

renderBlock ::
  BlockSize ->
  Max Word8 ->
  RegisterBlock ann ->
  [[Chunk]]
renderBlock blockSize maxAccountWidth RegisterBlock {..} =
  let blockLines =
        concatMap (renderEntry maxAccountWidth) (V.toList registerBlockEntries)
      totalLines =
        -- if registerBlockRunningTotal == MultiAccount.zero
        --   then [["0"], [" "]]
        --   else
        multiAccountChunksWithWidth Nothing registerBlockRunningTotal
      averageLines =
        multiAccountChunksWithWidth Nothing registerBlockRunningAverage
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

-- | Render a register entry (transaction or revaluation)
renderEntry ::
  Max Word8 ->
  RegisterEntry ann ->
  [[Chunk]]
renderEntry maxAccountWidth = \case
  RegisterEntryTransaction RegisterTransaction {..} ->
    let Located _ ts = registerTransactionTimestamp
     in renderTransaction
          maxAccountWidth
          ts
          (locatedValue <$> registerTransactionDescription)
          registerTransactionPostings
  RegisterEntryRevaluation reval ->
    renderRevaluation maxAccountWidth reval

-- | Render a revaluation entry
renderRevaluation ::
  Max Word8 ->
  RegisterRevaluation ann ->
  [[Chunk]]
renderRevaluation maxWidth RegisterRevaluation {..} =
  let Located _ ts = registerRevaluationTimestamp
      amountChunks = multiAccountChunksWithWidth (Just maxWidth) registerRevaluationAmount
      runningChunks = multiAccountChunksWithWidth (Just maxWidth) registerRevaluationBlockRunningTotal
      -- Pair up amount and running total lines
      renderLines = zipAmountAndRunning amountChunks runningChunks
   in hCatTable
        [ [[timestampChunk ts]],
          [[fore cyan $ chunk "Price change"]],
          renderLines
        ]
  where
    -- Combine amount and running total chunks into posting-like lines
    zipAmountAndRunning :: [[Chunk]] -> [[Chunk]] -> [[Chunk]]
    zipAmountAndRunning [] [] = [[chunk " ", chunk " ", chunk " ", chunk " ", chunk " "]]
    zipAmountAndRunning amts runs = go amts runs
      where
        go [] [] = []
        go (a : as) (r : rs) = ([chunk " "] ++ a ++ r) : go as rs
        go (a : as) [] = ([chunk " "] ++ a ++ [chunk " ", chunk " "]) : go as []
        go [] (r : rs) = ([chunk " ", chunk " ", chunk " "] ++ r) : go [] rs

registerMaxAccountWidth :: Register ann -> Max Word8
registerMaxAccountWidth Register {..} = foldMap goB registerBlocks
  where
    goB = foldMap goE . registerBlockEntries
    goE = \case
      RegisterEntryTransaction rt -> goT rt
      RegisterEntryRevaluation rr -> goR rr
    goT = foldMap goP . registerTransactionPostings
    goP RegisterPosting {..} =
      let Located _ Posting {..} = registerPosting
          Located _ a = postingAccount
       in accountWidth a <> multiAccountMaxWidth registerPostingBlockRunningTotal
    goR RegisterRevaluation {..} =
      multiAccountMaxWidth registerRevaluationAmount
        <> multiAccountMaxWidth registerRevaluationBlockRunningTotal

renderTransaction ::
  Max Word8 ->
  Timestamp ->
  Maybe Description ->
  Vector (RegisterPosting ann) ->
  [[Chunk]]
renderTransaction maxWidth timestamp mDescription postings =
  let postingLines =
        concatMap
          ( \RegisterPosting {..} ->
              let Located _ posting = registerPosting
               in renderPosting maxWidth posting registerPostingBlockRunningTotal
          )
          $ V.toList postings
   in hCatTable
        [ [[timestampChunk timestamp]],
          maybe [] descriptionChunks mDescription,
          postingLines
        ]

renderPosting ::
  Max Word8 ->
  Posting ann ->
  Money.MultiAccount (Currency ann) ->
  [[Chunk]]
renderPosting maxWidth Posting {..} runningTotal =
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
