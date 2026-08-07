module Centjes.MergeSpec (spec) where

import Centjes.Format (formatModule)
import Centjes.Location (GenLocated (..), locatedValue, noLoc)
import Centjes.Merge
import Centjes.Module (Declarations (..), Module (..), splitDeclarations, stripDeclarationAnnotation, stripModuleAnnotation, stripPriceDeclarationAnnotation, stripTransactionAnnotation)
import Centjes.Parse (parseModule)
import Centjes.Parse.TestUtils (shouldParse)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text.IO as T
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec = do
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
                  map (noLoc . stripPriceDeclarationAnnotation . locatedValue) $
                    declarationsPrices (splitDeclarations (moduleDeclarations newModule))
            pure $ formatModule $ mergePriceDeclarations existingDeclarations newDeclarations

  describe "mergeTransactionDeclarations" $
    scenarioDirRecur "test_resources/merge-transactions" $ \fp -> do
      af <- liftIO $ resolveFile' fp
      when (fileExtension af == Just ".cent" && toFilePath (filename af) == "existing.cent") $ do
        newFile <- liftIO $ replaceExtension ".cent" =<< resolveFile (parent af) "new"
        resultFile <- liftIO $ replaceExtension ".cent" =<< resolveFile (parent af) "result"
        let merged :: IO Text
            merged = do
              here <- getCurrentDir
              existingRf <- makeRelative here af
              existingContents <- T.readFile (fromAbsFile af)
              existingModule <- shouldParse parseModule here existingRf existingContents
              let existing = stripModuleAnnotation existingModule
              newRf <- makeRelative here newFile
              newContents <- T.readFile (fromAbsFile newFile)
              newModule <- shouldParse parseModule here newRf newContents
              let newTransactions =
                    map (noLoc . stripTransactionAnnotation . locatedValue) $
                      declarationsTransactions (splitDeclarations (moduleDeclarations newModule))
              pure $ formatModule $ mergeTransactionDeclarations existing newTransactions
        it "produces the same merged result" $
          goldenTextFile (fromAbsFile resultFile) merged
        -- An importer rewrites the file it reads, so a merge that does not
        -- round-trip through parse and format would churn the file forever.
        it "leaves the merged result alone when there is nothing new to add" $ do
          mergedContents <- merged
          here <- getCurrentDir
          resultRf <- makeRelative here resultFile
          mergedModule <- shouldParse parseModule here resultRf mergedContents
          formatModule (mergeTransactionDeclarations (stripModuleAnnotation mergedModule) []) `shouldBe` mergedContents
