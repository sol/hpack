{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Hpack.DependencySpec (spec) where

import           Helper
import           Test.HUnit

import           Data.Aeson.Types
import qualified Data.Map.Lazy as Map
import           Data.String.Interpolate.IsString
import           Data.Yaml
import           Data.ByteString (ByteString)

import           Hpack.Dependency

parsesAs :: HasCallStack => ByteString -> Either String [(String, DependencyVersion)] -> Expectation
parsesAs input expected = do
  value <- either assertFailure return (decodeEither input)
  parseEither parseJSON value `shouldBe` (Dependencies . Map.fromList <$> expected)


spec :: Spec
spec = do
  describe "parseJSON" $ do
    context "when parsing Dependencies" $ do
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

        it "accepts git dependencies" $ do
          let source = GitRef "https://github.com/sol/hpack" "master" Nothing
          [i|
            - name: hpack
              git: https://github.com/sol/hpack
              ref: master
          |] `parsesAs` Right [("hpack", SourceDependency source)]

        it "accepts github dependencies" $ do
          let source = GitRef "https://github.com/sol/hpack" "master" Nothing
          [i|
            - name: hpack
              github: sol/hpack
              ref: master
          |] `parsesAs` Right [("hpack", SourceDependency source)]

        it "accepts an optional subdirectory for git dependencies" $ do
          let source = GitRef "https://github.com/yesodweb/wai" "master" (Just "warp")
          [i|
            - name: warp
              github: yesodweb/wai
              ref: master
              subdir: warp
          |] `parsesAs` Right [("warp", SourceDependency source)]

        it "accepts local dependencies" $ do
          let source = Local "../hpack"
          [i|
            - name: hpack
              path: ../hpack
          |] `parsesAs` Right [("hpack", SourceDependency source)]

        context "when ref is missing" $ do
          it "produces accurate error messages" $ do
            [i|
              - name: hpack
                git: sol/hpack
                ef: master
            |] `parsesAs` Left "Error in $[0]: key \"ref\" not present"

        context "when both git and github are missing" $ do
          it "produces accurate error messages" $ do
            [i|
              - name: hpack
                gi: sol/hpack
                ref: master
            |] `parsesAs` Left "Error in $[0]: neither key \"git\" nor key \"github\" present"

      context "with a mapping from dependency names to constraints" $ do
        it "accepts dependencies without constraints" $ do
          [i|
            array:
          |] `parsesAs` Right [("array", AnyVersion)]

        it "rejects invalid values" $ do
          [i|
            hpack: 23
          |] `parsesAs` Left "Error in $.hpack: expected Null, Object, or String, encountered Number"

        context "when the constraint is a String" $ do
          it "accepts version ranges" $ do
            [i|
              hpack: '>=2'
            |] `parsesAs` Right [("hpack", VersionRange ">=2")]

        context "when the constraint is an Object" $ do
          it "accepts github dependencies" $ do
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

  describe "parseDependency" $ do
    it "parses a dependency" $ do
      parseDependency "foo" `shouldBe` Just ("foo", AnyVersion)

    it "parses a dependency with version range" $ do
      parseDependency "foo == 1.0" `shouldBe` Just ("foo", VersionRange "==1.0")
