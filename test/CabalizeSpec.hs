module CabalizeSpec (main, spec) where

import           Test.Hspec

import           ConfigSpec hiding (main, spec)
import           Config
import           Cabalize

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "renderPackage" $ do
    it "renders a package" $ do
      renderPackage 0 [] package `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns fields" $ do
      renderPackage 16 [] package `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes description" $ do
      renderPackage 0 [] package {packageDescription = Just "foo\nbar\n"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "description:"
        , "  foo"
        , "  ."
        , "  bar"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "includes source repository" $ do
      renderPackage 0 [] package {packageSourceRepository = Just "https://github.com/hspec/hspec"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        , ""
        , "source-repository head"
        , "  type: git"
        , "  location: https://github.com/hspec/hspec"
        ]

    context "when given list of existing fields" $ do
      it "retains field order" $ do
        renderPackage 16 ["cabal-version", "version", "name", "build-type"] package `shouldBe` unlines [
            "cabal-version:  >= 1.10"
          , "version:        0.0.0"
          , "name:           foo"
          , "build-type:     Simple"
          ]

      it "uses default field order for new fields" $ do
        renderPackage 16 ["name", "version", "cabal-version"] package `shouldBe` unlines [
            "name:           foo"
          , "version:        0.0.0"
          , "build-type:     Simple"
          , "cabal-version:  >= 1.10"
          ]
