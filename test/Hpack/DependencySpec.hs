{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Hpack.DependencySpec (spec) where

import           Helper
import           Test.HUnit

import           Data.Aeson.QQ
import           Data.Aeson.Types
import qualified Data.Map.Lazy as Map
import           Data.String.Interpolate.IsString
import           Data.Yaml
import           Data.ByteString (ByteString)

import           Hpack.Dependency

spec :: Spec
spec = do
  describe "parseJSON" $ do
    context "when parsing a Dependency" $ do
      context "when parsing source dependencies" $ do
        it "accepts git dependencies" $ do
          let value = [aesonQQ|{
                name: "hpack",
                git: "https://github.com/sol/hpack",
                ref: "master"
              }|]
              source = GitRef "https://github.com/sol/hpack" "master" Nothing
          parseEither parseJSON value `shouldBe` Right (Dependency "hpack" (SourceDependency source))

        it "accepts github dependencies" $ do
          let value = [aesonQQ|{
                name: "hpack",
                github: "sol/hpack",
                ref: "master"
              }|]
              source = GitRef "https://github.com/sol/hpack" "master" Nothing
          parseEither parseJSON value `shouldBe` Right (Dependency "hpack" (SourceDependency source))

        it "accepts an optional subdirectory for git dependencies" $ do
          let value = [aesonQQ|{
                name: "warp",
                github: "yesodweb/wai",
                ref: "master",
                subdir: "warp"
              }|]
              source = GitRef "https://github.com/yesodweb/wai" "master" (Just "warp")
          parseEither parseJSON value `shouldBe` Right (Dependency "warp" (SourceDependency source))

        it "accepts local dependencies" $ do
          let value = [aesonQQ|{
                name: "hpack",
                path: "../hpack"
              }|]
              source = Local "../hpack"
          parseEither parseJSON value `shouldBe` Right (Dependency "hpack" (SourceDependency source))

        context "when parsing fails" $ do
          it "returns an error message" $ do
            let value = Number 23
            parseEither parseJSON value `shouldBe` (Left "Error in $: expected String or an Object, encountered Number" :: Either String Dependency)

          context "when ref is missing" $ do
            it "produces accurate error messages" $ do
              let value = [aesonQQ|{
                    name: "hpack",
                    git: "sol/hpack",
                    ef: "master"
                  }|]
              parseEither parseJSON value `shouldBe` (Left "Error in $: key \"ref\" not present" :: Either String Dependency)

          context "when both git and github are missing" $ do
            it "produces accurate error messages" $ do
              let value = [aesonQQ|{
                    name: "hpack",
                    gi: "sol/hpack",
                    ref: "master"
                  }|]
              parseEither parseJSON value `shouldBe` (Left "Error in $: neither key \"git\" nor key \"github\" present" :: Either String Dependency)

    context "when parsing Dependencies" $ do
      let
        parsesAs :: HasCallStack => ByteString -> Either String [(String, DependencyVersion)] -> Expectation
        parsesAs input expected = do
          value <- either assertFailure return (decodeEither input)
          parseEither parseJSON value `shouldBe` (Dependencies . Map.fromList <$> expected)

      context "with a scalar" $ do
        it "accepts dependencies without constraints" $ do
          [i|
            hpack
          |] `parsesAs` Right [("hpack", AnyVersion)]

        it "accepts dependencies with constraints" $ do
          [i|
            hpack >= 2 && < 3
          |] `parsesAs` Right [("hpack", VersionRange ">=2 && <3")]

        context "with invalid constraint" $ do
          it "returns an error message" $ do
            [i|
              hpack ==
            |] `parsesAs` Left "Error in $: invalid dependency \"hpack ==\""

      context "with a list" $ do
        it "accepts dependencies without constraints" $ do
          [i|
            - hpack
          |] `parsesAs` Right [("hpack", AnyVersion)]

        it "accepts dependencies with constraints" $ do
          [i|
            - hpack >= 2 && < 3
          |] `parsesAs` Right [("hpack", VersionRange ">=2 && <3")]

        it "accepts an array of hashes" $ do
          [i|
            - name: hpack
              path: ../hpack
          |] `parsesAs` Right [("hpack", SourceDependency (Local "../hpack"))]

      context "with a hash" $ do
        it "accepts dependencies without constraints" $ do
          [i|
            array:
          |] `parsesAs` Right [("array", AnyVersion)]

        it "accepts dependencies with constraints" $ do
          [i|
            bytestring: 0.10.8.2
          |] `parsesAs` Right [("bytestring", VersionRange "0.10.8.2")]

        it "accepts a hash of hashes" $ do
          [i|
            Cabal:
              github: haskell/cabal
              ref: d53b6e0d908dfedfdf4337b2935519fb1d689e76
              subdir: Cabal
          |] `parsesAs` Right [("Cabal", SourceDependency (GitRef "https://github.com/haskell/cabal" "d53b6e0d908dfedfdf4337b2935519fb1d689e76" (Just "Cabal")))]

        it "ignores names in nested hashes" $ do
          [i|
            outer-name:
              name: inner-name
              path: somewhere
          |] `parsesAs` Right [("outer-name", SourceDependency (Local "somewhere"))]

        context "with invalid value" $ do
          it "returns an error message" $ do
            [i|
              hpack: 23
            |] `parsesAs` Left "Error in $.hpack: expected Null, Object, or String, encountered Number"

  describe "parseDependency" $ do
    it "parses a dependency" $ do
      parseDependency "foo" `shouldBe` Just ("foo", AnyVersion)

    it "parses a dependency with version range" $ do
      parseDependency "foo == 1.0" `shouldBe` Just ("foo", VersionRange "==1.0")
