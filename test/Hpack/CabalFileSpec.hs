module Hpack.CabalFileSpec (spec) where

import           Helper
import           Test.QuickCheck
import           Data.Version (showVersion)
import           Control.Monad.Compat

import           Paths_hpack (version)

import           Hpack (header)
import           Hpack.CabalFile

spec :: Spec
spec = do
  describe "readCabalFile" $ do
    let
      file = "package.yaml"
      hash = "some-hash"
    it "includes hash" $ do
      inTempDirectory $ do
        writeFile file $ header file version hash
        readCabalFile file `shouldReturn` CabalFile (Just version) (Just hash) []

  describe "extractVersion" $ do
    it "extracts Hpack version from a cabal file" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version 0.10.0."]
      extractVersion cabalFile `shouldBe` Just (makeVersion [0, 10, 0])

    it "is total" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version "]
      extractVersion cabalFile `shouldBe` Nothing

  describe "parseVersion" $ do
    it "is inverse to showVersion" $ do
      let positive = getPositive <$> arbitrary
      forAll (replicateM 3 positive) $ \xs -> do
        let v = makeVersion xs
        parseVersion (showVersion v) `shouldBe` Just v
