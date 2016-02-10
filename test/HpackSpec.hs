module HpackSpec (spec) where

import           Test.Hspec

import           Hpack

spec :: Spec
spec = do
  describe "parseVerbosity" $ do
    it "returns True by default" $ do
      parseVerbosity ["foo"] `shouldBe` (True, ["foo"])

    context "with --silent" $ do
      it "returns False" $ do
        parseVerbosity ["--silent"] `shouldBe` (False, [])
