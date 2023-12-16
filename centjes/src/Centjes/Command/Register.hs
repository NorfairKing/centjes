{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Centjes.Command.Register
  ( runCentjesRegister,
  )
where

import Centjes.Compile
import Centjes.Formatting
import Centjes.Ledger
import Centjes.Load
import Centjes.Location
import Centjes.OptParse
import Centjes.Report.Register
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
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout

runCentjesRegister :: Settings -> RegisterSettings -> IO ()
runCentjesRegister Settings {..} RegisterSettings {..} = runStderrLoggingT $ do
  (declarations, diag) <- loadModules settingLedgerFile
  ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations
  register <- liftIO $ checkValidation diag $ produceRegister registerSettingCurrency ledger
  terminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  let t = table (renderRegister register)
  liftIO $ putChunksLocaleWith terminalCapabilities $ renderTable t

renderRegister :: Register ann -> [[Chunk]]
renderRegister r@(Register v) =
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
  let headerChunks =
        [ fore white $ chunk $ T.pack $ show ix,
          timestampChunk timestamp,
          maybe " " descriptionChunk mDescription
        ]

      postingLines =
        concatMap
          ( \(Located _ posting, runningTotal) ->
              renderPosting maxWidth posting runningTotal
          )
          $ V.toList postings
   in case postingLines of
        [] -> [headerChunks]
        (l : ls) -> (headerChunks ++ l) : map ([chunk " ", chunk " ", chunk " "] ++) ls

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
