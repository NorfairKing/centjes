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
import Centjes.Validation
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Colour.Layout

runCentjesRegister :: Settings -> RegisterSettings -> IO ()
runCentjesRegister Settings {..} RegisterSettings = runStderrLoggingT $ do
  (declarations, diag) <- loadModules settingLedgerFile
  ledger <- liftIO $ checkValidation diag $ compileDeclarations declarations
  terminalCapabilities <- liftIO getTerminalCapabilitiesFromEnv
  let t = table (renderRegister ledger)
  liftIO $ putChunksLocaleWith terminalCapabilities $ renderTable t

renderRegister :: Ledger ann -> [[Chunk]]
renderRegister Ledger {..} =
  concatMap
    (\(ix, Located _ t) -> renderTransaction ix t)
    (V.indexed ledgerTransactions)

renderTransaction ::
  Int ->
  Transaction ann ->
  [[Chunk]]
renderTransaction ix Transaction {..} =
  let Located _ timestamp = transactionTimestamp
      headerChunks =
        [ fore white $ chunk $ T.pack $ show ix,
          timestampChunk timestamp,
          maybe " " (descriptionChunk . locatedValue) transactionDescription
        ]

      postingLines = map renderPosting transactionPostings
   in case postingLines of
        [] -> [headerChunks]
        (l : ls) -> (headerChunks ++ l) : map ([chunk " ", chunk " ", chunk " "] ++) ls

renderPosting ::
  GenLocated ann (Posting ann) ->
  [Chunk]
renderPosting (Located _ Posting {..}) =
  let Located _ accountName = postingAccountName
      Located _ Currency {..} = postingCurrency
      Located _ quantisationFactor = currencyQuantisationFactor
   in [ accountNameChunk accountName,
        accountChunk quantisationFactor (locatedValue postingAccount),
        currencySymbolChunk currencySymbol
      ]
