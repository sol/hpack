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
      renderPackage package `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "includes description" $ do
      renderPackage package {packageDescription = Just "foo\nbar\n"} `shouldBe` unlines [
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
      renderPackage package {packageSourceRepository = Just "https://github.com/hspec/hspec"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        , "source-repository head"
        , "  type: git"
        , "  location: https://github.com/hspec/hspec"
        ]
