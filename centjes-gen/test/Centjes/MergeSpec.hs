module Centjes.MergeSpec (spec) where

import Centjes.Format (formatModule)
import Centjes.Location (GenLocated (..), locatedValue, noLoc)
import Centjes.Merge
import Centjes.Module (Declaration (..), Module (..), stripDeclarationAnnotation, stripPriceDeclarationAnnotation)
import Centjes.Parse (parseModule)
import Centjes.Parse.TestUtils (shouldParse)
import Control.Monad
import qualified Data.Text.IO as T
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec =
  describe "mergePriceDeclarations" $
    scenarioDirRecur "test_resources/merge" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent" && toFilePath (filename af) == "existing.cent") $ do
        newFile <- liftIO $ replaceExtension ".cent" =<< resolveFile (parent af) "new"
        resultFile <- liftIO $ replaceExtension ".cent" =<< resolveFile (parent af) "result"
        it "produces the same merged result" $
          goldenTextFile (fromAbsFile resultFile) $ do
            here <- getCurrentDir
            existingRf <- makeRelative here af
            existingContents <- T.readFile (fromAbsFile af)
            existingModule <- shouldParse parseModule here existingRf existingContents
            let existingDeclarations = map (noLoc . stripDeclarationAnnotation . locatedValue) (moduleDeclarations existingModule)
            newRf <- makeRelative here newFile
            newContents <- T.readFile (fromAbsFile newFile)
            newModule <- shouldParse parseModule here newRf newContents
            let newDeclarations =
                  [ noLoc (stripPriceDeclarationAnnotation pd)
                  | Located _ (DeclarationPrice (Located _ pd)) <- moduleDeclarations newModule
                  ]
            pure $ formatModule $ mergePriceDeclarations existingDeclarations newDeclarations
