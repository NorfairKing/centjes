module Centjes.Switzerland.Reporter where

import Centjes.Validation
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as M
import Path

-- | A report that collects the files it includes, and is pure, so that other
-- reports can be computed from it.
type Reporter e a =
  ValidationT
    e
    (Writer (Map (Path Rel File) (Path Rel File)))
    a

includeFile :: Path Rel File -> Path Rel File -> Reporter e ()
includeFile locationInTarball locationOnDisk =
  lift $ tell $ M.singleton locationInTarball locationOnDisk

runReporter ::
  Reporter e a ->
  Validation e (a, Map (Path Rel File) (Path Rel File))
runReporter reporter =
  let (v, fs) = runWriter (runValidationT reporter)
   in (,) <$> v <*> pure fs
