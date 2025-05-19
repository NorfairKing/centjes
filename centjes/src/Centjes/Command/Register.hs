{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Register
  ( runCentjesRegister,
    renderRegister,
  )
where

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
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import qualified Money.Account as Account
import qualified Money.MultiAccount as Money (MultiAccount)
import Text.Colour
import Text.Colour.Layout

runCentjesRegister :: Settings -> RegisterSettings -> LoggingT IO ()
runCentjesRegister Settings {..} RegisterSettings {..} =
  loadMWatchedModules settingWatch settingLedgerFile $ \(declarations, diagnostic) -> do
    ledger <- withLoggedDuration "Compile" $ liftIO $ checkValidation diagnostic $ compileDeclarations declarations
    register <-
      withLoggedDuration "Produce register" $
        liftIO $
          checkValidation diagnostic $
            produceRegister
              registerSettingFilter
              registerSettingCurrency
              registerSettingShowVirtual
              registerSettingBegin
              registerSettingEnd
              ledger
    liftIO $ putChunksLocaleWith settingTerminalCapabilities $ renderRegister register

renderRegister :: Register ann -> [Chunk]
renderRegister register =
  let t = table (map (map pure) (renderRegisterTable register))
   in renderTable t

renderRegisterTable :: Register ann -> [[Chunk]]
renderRegisterTable r@(Register v) =
  let maxAccountWidth = registerMaxAccountWidth r
   in concatMap
        ( \(ix, (Located _ ts, mDescription, postings)) ->
            renderTransaction
              maxAccountWidth
              ix
              ts
              (locatedValue <$> mDescription)
              postings
        )
        (V.indexed v)

registerMaxAccountWidth :: Register ann -> Max Word8
registerMaxAccountWidth (Register v) = foldMap goT v
  where
    goT (_, _, ps) = foldMap goP ps
    goP (Located _ Posting {..}, ma) =
      let Located _ a = postingAccount
       in accountWidth a <> multiAccountMaxWidth ma

renderTransaction ::
  Max Word8 ->
  Int ->
  Timestamp ->
  Maybe Description ->
  Vector
    ( GenLocated ann (Posting ann),
      Money.MultiAccount (Currency ann)
    ) ->
  [[Chunk]]
renderTransaction maxWidth ix timestamp mDescription postings =
  let postingLines =
        concatMap
          ( \(Located _ posting, runningTotal) ->
              renderPosting maxWidth posting runningTotal
          )
          $ V.toList postings
   in hCatTable
        [ [[fore white $ chunk $ T.pack $ show ix]],
          [[timestampChunk timestamp]],
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
