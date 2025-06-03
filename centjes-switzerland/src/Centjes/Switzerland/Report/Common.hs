module Centjes.Switzerland.Report.Common where

import Centjes.Ledger
import Centjes.Location
import qualified Data.Vector as V
import Path

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
