module Centjes.Switzerland.Report.Common where

import Centjes.Ledger
import Centjes.Location
import Control.Monad
import qualified Data.Vector as V
import Path
import Path.IO

withPacketDir :: Bool -> Maybe (Path Abs Dir) -> (Path Abs Dir -> IO a) -> IO a
withPacketDir clean mDir func = case mDir of
  Nothing -> withSystemTempDir "centjes-switzerland" func
  Just dir -> do
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
