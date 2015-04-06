module CabalizeSpec (main, spec) where

import           Test.Hspec

import           ConfigSpec hiding (main, spec)
import           Cabalize

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "renderPackage" $ do
    it "renders a package description" $ do
      renderPackage package `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]
