{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Centjes
  ( runCentjes,
  )
where

import Centjes.Format
import Centjes.Module
import Centjes.OptParse
import Centjes.Parse
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Data.ByteString as SB
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import GHC.Generics (Generic)
import qualified Money.Account as Account
import qualified Money.Account as Money (Account)
import Path
import Path.IO
import System.Exit

runCentjes :: IO ()
runCentjes = do
  Instructions (DispatchFormat FormatSettings {..}) Settings <- getInstructions
  runStderrLoggingT $ do
    case formatSettingFileOrDir of
      Nothing -> getCurrentDir >>= formatDir
      Just (Right d) -> formatDir d
      Just (Left f) -> formatFile f
  doExample

formatDir :: Path Abs Dir -> LoggingT IO ()
formatDir = walkDir $ \_ _ files -> do
  mapM_ formatFile files
  pure $ WalkExclude [] -- Exclude hidden dirs and files?

formatFile :: Path Abs File -> LoggingT IO ()
formatFile fp =
  case fileExtension fp of
    Just ".cent" -> do
      contents <- liftIO $ SB.readFile (fromAbsFile fp)
      case TE.decodeUtf8' contents of
        Left err ->
          logWarnN $
            T.pack $
              unlines
                [ "Could not format file because it does not look like Utf-8: ",
                  show fp,
                  show err
                ]
        Right textContents -> do
          case parseModule (fromAbsFile fp) textContents of
            Left err ->
              liftIO $
                die $
                  unlines
                    [ "Cannot parse file: ",
                      show fp,
                      err
                    ]
            Right m -> do
              let newTextContents = formatModule m
              case parseModule "idempotence-test" newTextContents of
                Left err ->
                  liftIO $
                    die $
                      unlines
                        [ "Formatted file could not be parsed, this indicates a bug:",
                          err,
                          T.unpack newTextContents
                        ]
                Right m' ->
                  when (m /= m') $
                    liftIO $
                      die $
                        unlines
                          [ "Formatting was not idempotent.",
                            "Before:",
                            show m,
                            "After:",
                            show m'
                          ]
              when (newTextContents /= textContents) $
                liftIO $
                  SB.writeFile (fromAbsFile fp) $
                    TE.encodeUtf8 newTextContents
              logInfoN $ T.pack $ unwords ["Formatted", fromAbsFile fp]
    _ -> pure ()

doExample :: IO ()
doExample = do
  let exampleTransaction =
        Transaction
          { transactionTimestamp = fromGregorian 2013 11 13,
            transactionDescription = Description "Example",
            transactionPostings =
              [ Posting
                  { postingAccountName = AccountName "expenses:food",
                    postingAmount = fromJust $ Account.fromMinimalQuantisations 100
                  },
                Posting
                  { postingAccountName = AccountName "assets:cash",
                    postingAmount = fromJust $ Account.fromMinimalQuantisations (-100)
                  }
              ]
          }
  print $ balanceTransaction exampleTransaction

data Balancing
  = Balanced
  | OffBalanceBy !Money.Account
  | CouldNotBalance
  deriving stock (Show, Eq, Generic)

balanceTransaction :: Transaction -> Balancing
balanceTransaction Transaction {..} =
  let actualSum = Account.sum (map postingAmount transactionPostings)
   in case actualSum of
        Nothing -> CouldNotBalance
        Just s ->
          if s == Account.zero
            then Balanced
            else OffBalanceBy s
