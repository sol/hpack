module Hpack.DependencySpec (spec) where

import           Test.Hspec

import           Hpack.Dependency

spec :: Spec
spec = do
  describe "parseDependency" $ do
    it "parses a dependency" $ do
      parseDependency "foo" `shouldBe` Just ("foo", Nothing)

    it "parses a dependency with version range" $ do
      parseDependency "foo == 1.0" `shouldBe` Just ("foo", Just "==1.0")
