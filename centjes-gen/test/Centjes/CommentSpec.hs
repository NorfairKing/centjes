{-# LANGUAGE TypeApplications #-}

module Centjes.CommentSpec (spec) where

import Centjes.Comment as Comment
import Centjes.Comment.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Comment

  describe "fromText" $
    it "produces valid comments" $
      producesValid Comment.fromText
