module HpackSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Compat
import           Control.DeepSeq
import           Data.Version (Version (..), showVersion)

import           Test.Hspec
import           Test.Mockery.Directory
import           Test.QuickCheck

import           Hpack
import           Hpack.Config
import           Hpack.Convert

makeVersion :: [Int] -> Version
makeVersion v = Version v []

spec :: Spec
spec = do
  describe "parseVerbosity" $ do
    it "returns True by default" $ do
      parseVerbosity ["foo"] `shouldBe` (True, ["foo"])

    context "with --silent" $ do
      it "returns False" $ do
        parseVerbosity ["--silent"] `shouldBe` (False, [])

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
          hpackWithVersion (makeVersion [0,8,0]) "." False
          old <- readFile "foo.cabal" >>= (return $!!)
          hpackWithVersion (makeVersion [0,10,0]) "." False
          readFile "foo.cabal" `shouldReturn` old

    context "when exsting cabal file was generated with a newer version of hpack" $ do
      it "does not re-generate" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "version: 0.1.0"
            ]
          hpackWithVersion (makeVersion [0,10,0]) "." False
          old <- readFile "foo.cabal" >>= (return $!!)

          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "version: 0.2.0"
            ]

          hpackWithVersion (makeVersion [0,8,0]) "." False
          readFile "foo.cabal" `shouldReturn` old
