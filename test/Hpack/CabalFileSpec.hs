{-# LANGUAGE QuasiQuotes #-}
module Hpack.CabalFileSpec (spec) where

import           Helper
import           Test.QuickCheck
import           Data.Version (showVersion)
import           Data.String.Interpolate
import           Data.String.Interpolate.Util

import           Paths_hpack (version)

import           Hpack.Util (Hash)
import           Data.Version (Version)
import           Hpack (header)

import           Hpack.CabalFile

mkHeader :: FilePath -> Version -> Hash -> String
mkHeader p v hash = unlines $ header p (Just v) (Just hash)

spec :: Spec
spec = do
  describe "readCabalFile" $ do
    let
      file = "hello.cabal"
      hash = "some-hash"

    it "includes hash" $ do
      inTempDirectory $ do
        writeFile file $ mkHeader "package.yaml" version hash
        readCabalFile file `shouldReturn` Just (CabalFile [] (Just version) (Just hash) [] DoesNotHaveGitConflictMarkers)

    it "accepts cabal-version at the beginning of the file" $ do
      inTempDirectory $ do
        writeFile file $ ("cabal-version: 2.2\n" ++ mkHeader "package.yaml" version hash)
        readCabalFile file `shouldReturn` Just (CabalFile ["cabal-version: 2.2"] (Just version) (Just hash) [] DoesNotHaveGitConflictMarkers)

  describe "extractVersion" $ do
    it "extracts Hpack version from a cabal file" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version 0.10.0."]
      extractVersion cabalFile `shouldBe` Just (makeVersion [0, 10, 0])

    it "is agnostic to file name" $ do
      let cabalFile = ["-- This file has been generated from some random file by hpack version 0.10.0."]
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

  describe "removeGitConflictMarkers" $ do
    it "remove git conflict markers (git checkout --ours)" $ do
      let
        input = lines $ unindent [i|
          foo
          <<<<<<< 4a1ca1694ed77195a080688df9bef53c23045211
          bar2
          =======
          bar1
          >>>>>>> update foo on branch foo
          baz
          |]
        expected = lines $ unindent [i|
          foo
          bar2
          baz
          |]
      removeGitConflictMarkers input `shouldBe` expected
