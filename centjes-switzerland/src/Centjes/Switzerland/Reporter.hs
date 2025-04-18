module Centjes.Switzerland.Reporter where

import Centjes.Validation
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M
import Path

type Reporter e a =
  ValidationT
    e
    (WriterT (Map (Path Rel File) (Path Rel File)) IO)
    a

includeFile :: Path Rel File -> Path Rel File -> Reporter e ()
includeFile locationInTarball locationOnDisk =
  lift $ tell $ M.singleton locationInTarball locationOnDisk

runReporter ::
  Reporter e a ->
  ValidationT e IO (a, Map (Path Rel File) (Path Rel File))
runReporter =
  transformValidationT
    ( \f -> do
        (v, fs) <- runWriterT f
        pure $ (,) <$> v <*> pure fs
    )
