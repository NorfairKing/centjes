module Centjes.Switzerland.TestUtils (dirScenarioDir) where

import Control.Monad
import Data.Maybe
import Path
import Path.IO
import qualified System.FilePath as FP
import Test.Syd

dirScenarioDir :: FilePath -> (FilePath -> TestDefM outers inner ()) -> TestDefM outers inner ()
dirScenarioDir dp func = do
  describe dp $ do
    ad <- liftIO $ resolveDir' dp
    fs <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ fst <$> listDirRel ad
    forM_ fs $ \rf -> do
      let fp = dp FP.</> fromRelDir rf
      describe (fromRelDir rf) $ func fp
