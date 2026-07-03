{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Centjes.Switzerland.Report.VAT.TypesSpec (spec) where

import Centjes.Switzerland.Report.VAT
import Centjes.Switzerland.Report.VAT.Gen ()
import Data.Either (isLeft)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @VATId

  describe "parseVATId" $ do
    it "parses a plain dotted identifier" $
      parseVATId "111.222.333" `shouldBe` Right (VATId "111222333")

    it "parses an identifier with a CHE prefix and dashes" $
      parseVATId "CHE-111.222.333" `shouldBe` Right (VATId "111222333")

    it "parses an identifier with a CHE prefix and no separators" $
      parseVATId "CHE111222333" `shouldBe` Right (VATId "111222333")

    it "parses a lower-case che prefix with spaces" $
      parseVATId "che 111 222 333" `shouldBe` Right (VATId "111222333")

    it "parses nine bare digits" $
      parseVATId "111222333" `shouldBe` Right (VATId "111222333")

    it "rejects an identifier that is too short" $
      parseVATId "111.222.33" `shouldSatisfy` isLeft

    it "rejects an identifier that is too long" $
      parseVATId "111.222.3334" `shouldSatisfy` isLeft

    it "rejects an identifier that starts with a zero" $
      parseVATId "011.222.333" `shouldSatisfy` isLeft

    it "rejects an identifier with a non-digit" $
      parseVATId "111.222.33a" `shouldSatisfy` isLeft

    it "roundtrips with renderVATIdReference" $
      forAllValid $ \vatId ->
        parseVATId (renderVATIdReference vatId) `shouldBe` Right vatId

  describe "vatIdToken" $
    it "prepends the CHE category prefix" $
      vatIdToken (VATId "111222333") `shouldBe` "CHE111222333"
