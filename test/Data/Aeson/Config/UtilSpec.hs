module Data.Aeson.Config.UtilSpec (spec) where

import           Test.Hspec

import           Data.Aeson.Config.Util

spec :: Spec
spec = do
  describe "hyphenize" $ do
    it "hyphenizes" $ do
      hyphenize "" "personFirstName" `shouldBe` "person-first-name"

    it "ignores leading underscores" $ do
      hyphenize "" "__personFirstName" `shouldBe` "person-first-name"

    context "when given a type name" $ do
      it "strips type name" $ do
        hyphenize "Person" "personFirstName" `shouldBe` "first-name"

      it "ignores trailing underscores in type name" $ do
        hyphenize "Person__" "personFirstName" `shouldBe` "first-name"
