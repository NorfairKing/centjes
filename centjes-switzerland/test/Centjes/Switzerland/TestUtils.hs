module Centjes.Switzerland.TestUtils (dirScenarioDir) where

import Control.Monad
import Data.Maybe
import Path
import Path.IO
import Test.Syd

dirScenarioDir :: Path Rel Dir -> (Path Rel Dir -> TestDefM outers inner ()) -> TestDefM outers inner ()
dirScenarioDir rd func = do
  describe (fromRelDir rd) $ do
    ad <- liftIO $ makeAbsolute rd
    fs <- liftIO $ fmap (fromMaybe []) $ forgivingAbsence $ fst <$> listDirRel ad
    forM_ fs $ \rf ->
      describe (fromRelDir rf) $ func (rd </> rf)
