module HpackSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Control.DeepSeq
import           Data.Version (Version(..), showVersion)

import           Test.Hspec
import           Test.Mockery.Directory
import           Test.QuickCheck

import           Hpack
import           Hpack.OptionalExposure (BuildPlan(..))

makeVersion :: [Int] -> Version
makeVersion v = Version v []

maxBuild :: Either BuildPlan a
maxBuild = Left MaximalBuildPlan

spec :: Spec
spec = do
  describe "parseVerbosity" $ do
    it "returns True by default" $ do
      parseVerbosity ["foo"] `shouldBe` (True, ["foo"])
    context "with --silent" $ do
      it "returns False" $ do
        parseVerbosity ["--silent"] `shouldBe` (False, [])

  describe "parseBuildplan" $ do
    it "returns MaximalBuildPlan by default" $ do
      parseBuildplan ["foo","bar"] `shouldBe` (Left MaximalBuildPlan, ["foo","bar"])
    it "accepts \"min\" as a build plan" $ do
      parseBuildplan ["foo", "--buildplan", "min", "bar"]
        `shouldBe` (Left MinimalBuildPlan, ["foo", "bar"])
    it "returns the build plan file when available" $ do
      parseBuildplan ["foo", "--buildplan", "baz", "bar"]
        `shouldBe` (Right "baz", ["foo", "bar"])
  describe "extractVersion" $ do
    it "extracts Hpack version from a cabal file" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version 0.10.0."]
      extractVersion cabalFile `shouldBe` Just (Version [0, 10, 0] [])

    it "is total" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version "]
      extractVersion cabalFile `shouldBe` Nothing

  describe "parseVersion" $ do
    it "is inverse to showVersion" $ do
      let positive = getPositive <$> arbitrary
      forAll (replicateM 3 positive) $ \xs -> do
        let v = Version xs []
        parseVersion (showVersion v) `shouldBe` Just v

  describe "hpackWithVersion" $ do
    context "when only the hpack version in the cabal file header changed" $ do
      it "does not write a new cabal file" $ do
        inTempDirectory $ do
          writeFile "package.yaml" "name: foo"
          hpackWithVersion (makeVersion [0,8,0]) maxBuild "." False
          old <- readFile "foo.cabal" >>= (return $!!)
          hpackWithVersion (makeVersion [0,10,0]) maxBuild "." False
          readFile "foo.cabal" `shouldReturn` old

    context "when exsting cabal file was generated with a newer version of hpack" $ do
      it "does not re-generate" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "version: 0.1.0"
            ]
          hpackWithVersion (makeVersion [0,10,0]) maxBuild "." False
          old <- readFile "foo.cabal" >>= (return $!!)

          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "version: 0.2.0"
            ]

          hpackWithVersion (makeVersion [0,8,0]) maxBuild "." False
          readFile "foo.cabal" `shouldReturn` old
