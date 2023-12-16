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
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Money.Account as Account
import qualified Money.MultiAccount as Money (MultiAccount)
import qualified Money.MultiAccount as MultiAccount
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
renderRegister (Register v) =
  concatMap
    ( \(ix, (Located _ ts, mDescription, postings)) ->
        renderTransaction
          ix
          ts
          (locatedValue <$> mDescription)
          postings
    )
    (V.indexed v)

renderTransaction ::
  Int ->
  Timestamp ->
  Maybe Description ->
  Vector
    ( GenLocated ann (Posting ann),
      Money.MultiAccount (Currency ann)
    ) ->
  [[Chunk]]
renderTransaction ix timestamp mDescription postings =
  let headerChunks =
        [ fore white $ chunk $ T.pack $ show ix,
          timestampChunk timestamp,
          maybe " " descriptionChunk mDescription
        ]

      postingLines =
        concatMap
          ( \(Located _ posting, runningTotal) ->
              renderPosting posting runningTotal
          )
          $ V.toList postings
   in case postingLines of
        [] -> [headerChunks]
        (l : ls) -> (headerChunks ++ l) : map ([chunk " ", chunk " ", chunk " "] ++) ls

renderPosting ::
  Posting ann ->
  Money.MultiAccount (Currency ann) ->
  [[Chunk]]
renderPosting Posting {..} runningTotal =
  let Located _ accountName = postingAccountName
      Located _ Currency {..} = postingCurrency
      Located _ quantisationFactor = currencyQuantisationFactor
      Located _ acc = postingAccount
      f = fore $ if acc >= Account.zero then green else red
      postingChunks =
        [ accountNameChunk accountName,
          f $ accountChunk quantisationFactor acc,
          f $ currencySymbolChunk currencySymbol
        ]
      totalChunks = multiAccountChunks runningTotal
   in case totalChunks of
        [] -> [postingChunks]
        (cs : css) -> (postingChunks ++ cs) : map ([" ", " ", " "] ++) css
