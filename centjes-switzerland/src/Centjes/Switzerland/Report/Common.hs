module Centjes.Switzerland.Report.Common where

import Centjes.Ledger
import Centjes.Location
import Control.Monad
import qualified Data.Vector as V
import Path
import Path.IO
import UnliftIO

{-# ANN withPacketDir ("NOCOVER" :: String) #-}
withPacketDir :: (MonadUnliftIO m) => Bool -> Maybe (Path Abs Dir) -> (Path Abs Dir -> m a) -> m a
withPacketDir clean mDir func = case mDir of
  Nothing -> withRunInIO $ \runInIO -> withSystemTempDir "centjes-switzerland" $ \dir -> runInIO $ func dir
  Just dir -> do
    liftIO $ do
      when clean $ ignoringAbsence $ removeDirRecur dir
      ensureDir dir
    func dir

filterLedgerByPricesFile :: Path Rel File -> Ledger SourceSpan -> Ledger SourceSpan
filterLedgerByPricesFile pricesFile ledger =
  let pricesFromFile =
        V.filter
          ( \(Located loc _) ->
              let priceFile = sourceSpanFile loc
               in priceFile == pricesFile
          )
          (ledgerPrices ledger)
   in ledger {ledgerPrices = pricesFromFile}
