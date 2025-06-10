module Centjes.Switzerland.Report.Common
  ( withPacketDir,
    filterLedgerByPricesFile,
    DeductibleDeclaration (..),
    decideDeductible,
  )
where

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

data DeductibleDeclaration ann
  = DefinitelyDeductible ann
  | DefinitelyNotDeductible ann
  | Undeclared
  | RedundantlyDeclared ann ann
  | AmbiguouslyDeclared ann ann

decideDeductible ::
  Maybe ann ->
  Maybe ann ->
  Maybe ann ->
  Maybe ann ->
  DeductibleDeclaration ann
decideDeductible
  mGloballyDeductible
  mGloballyNotDeductible
  mLocallyDeductible
  mLocallyNotDeductible =
    case (mGloballyDeductible, mGloballyNotDeductible, mLocallyDeductible, mLocallyNotDeductible) of
      (Nothing, Nothing, Nothing, Nothing) -> Undeclared
      (Just l, Nothing, Nothing, Nothing) -> DefinitelyDeductible l
      (Nothing, Just l, Nothing, Nothing) -> DefinitelyNotDeductible l
      (Nothing, Nothing, Just l, Nothing) -> DefinitelyDeductible l
      (Nothing, Nothing, Nothing, Just l) -> DefinitelyNotDeductible l
      (Just lg, Nothing, Just ll, Nothing) -> RedundantlyDeclared lg ll
      (Nothing, Just lg, Nothing, Just ll) -> RedundantlyDeclared lg ll
      (Just ly, Just ln, _, Nothing) -> AmbiguouslyDeclared ly ln
      (Just ly, _, _, Just ln) -> AmbiguouslyDeclared ly ln
      (Nothing, Nothing, Just ly, Just ln) -> AmbiguouslyDeclared ly ln
      (Nothing, Just ln, Just ly, _) -> AmbiguouslyDeclared ly ln
