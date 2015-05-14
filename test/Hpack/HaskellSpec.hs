module Hpack.HaskellSpec (spec) where

import           Test.Hspec

import           Hpack.Haskell

spec :: Spec
spec = do
  describe "isModule" $ do
    it "accepts module names" $ do
      isModule ["Foo", "Bar"] `shouldBe` True

    it "rejects the empty list" $ do
      isModule [] `shouldBe` False

  describe "isQualifiedIdentifier" $ do
    it "accepts qualified Haskell identifiers" $ do
      isQualifiedIdentifier ["Foo", "Bar", "baz"] `shouldBe` True

    it "rejects invalid input" $ do
      isQualifiedIdentifier ["Foo", "Bar", "Baz"] `shouldBe` False

  describe "isIdentifier" $ do
    it "accepts Haskell identifiers" $ do
      isIdentifier "foo" `shouldBe` True

    it "rejects reserved keywords" $ do
      isIdentifier "case" `shouldBe` False

    it "rejects invalid input" $ do
      isIdentifier "Foo" `shouldBe` False
