{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Hpack.Syntax.DependenciesSpec (spec) where

import           Helper

import           Data.Aeson.Config.FromValueSpec (shouldDecodeTo, shouldDecodeTo_)

import           Data.Aeson.Config.FromValue
import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.Dependencies

left :: String -> Result Dependencies
left = Left

defaultInfo :: DependencyInfo
defaultInfo = DependencyInfo [] anyVersion

spec :: Spec
spec = do
  describe "parseDependency" $ do
    it "accepts dependencies" $ do
      parseDependency "dependency" "foo" `shouldReturn` ("foo", DependencyVersion Nothing AnyVersion)

    it "accepts dependencies with a subcomponent" $ do
      parseDependency "dependency" "foo:bar" `shouldReturn` ("foo:bar", DependencyVersion Nothing AnyVersion)

    it "accepts dependencies with multiple subcomponents" $ do
      parseDependency "dependency" "foo:{bar,baz}" `shouldReturn` ("foo:{bar,baz}", DependencyVersion Nothing AnyVersion)

  describe "fromValue" $ do
    context "when parsing Dependencies" $ do
      context "with a scalar" $ do
        it "accepts dependencies without constraints" $ do
          [yaml|
            hpack
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo)]

        it "accepts dependencies with constraints" $ do
          [yaml|
            hpack >= 2 && < 4
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange ">=2 && <4" })]

        context "with invalid constraint" $ do
          it "returns an error message" $ do
            [yaml|
              hpack ==
            |] `shouldDecodeTo` left "Error while parsing $ - invalid dependency \"hpack ==\""

      context "with a list" $ do
        it "accepts dependencies without constraints" $ do
          [yaml|
            - hpack
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo)]

        it "accepts dependencies with constraints" $ do
          [yaml|
            - hpack >= 2 && < 4
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange ">=2 && <4" })]

        it "accepts ^>=" $ do
          [yaml|
            - hpack ^>= 1.2.3.4
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange ">=1.2.3.4 && <1.3" })]

        it "accepts objects with name and version" $ do
          [yaml|
            - name: hpack
              version: 0.1.0
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==0.1.0" })]

        it "accepts git dependencies with version" $ do
          let source = Just (GitRef "https://github.com/sol/hpack" "master" Nothing)
          [yaml|
            - name: hpack
              version: 0.1.0
              git: https://github.com/sol/hpack
              ref: master
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = DependencyVersion source (VersionRange "==0.1.0") })]

        it "accepts git dependencies" $ do
          let source = Just (GitRef "https://github.com/sol/hpack" "master" Nothing)
          [yaml|
            - name: hpack
              git: https://github.com/sol/hpack
              ref: master
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = DependencyVersion source AnyVersion })]

        it "accepts github dependencies" $ do
          let source = Just (GitRef "https://github.com/sol/hpack" "master" Nothing)
          [yaml|
            - name: hpack
              github: sol/hpack
              ref: master
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = DependencyVersion source AnyVersion })]

        it "accepts an optional subdirectory for git dependencies" $ do
          let source = Just (GitRef "https://github.com/yesodweb/wai" "master" (Just "warp"))
          [yaml|
            - name: warp
              github: yesodweb/wai
              ref: master
              subdir: warp
          |] `shouldDecodeTo_` Dependencies [("warp", defaultInfo { dependencyInfoVersion = DependencyVersion source AnyVersion })]

        it "accepts local dependencies" $ do
          let source = Just (Local "../hpack")
          [yaml|
            - name: hpack
              path: ../hpack
          |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo {dependencyInfoVersion = DependencyVersion source AnyVersion })]

        context "when ref is missing" $ do
          it "produces accurate error messages" $ do
            [yaml|
              - name: hpack
                git: sol/hpack
                ef: master
            |] `shouldDecodeTo` left "Error while parsing $[0] - key \"ref\" not present"

        context "when both git and github are missing" $ do
          it "produces accurate error messages" $ do
            [yaml|
              - name: hpack
                gi: sol/hpack
                ref: master
            |] `shouldDecodeTo` left "Error while parsing $[0] - neither key \"git\" nor key \"github\" present"

      context "with a mapping from dependency names to constraints" $ do
        it "accepts dependencies without constraints" $ do
          [yaml|
            array:
          |] `shouldDecodeTo_` Dependencies [("array", defaultInfo)]

        it "rejects invalid values" $ do
          [yaml|
            hpack: []
          |] `shouldDecodeTo` left "Error while parsing $.hpack - expected Null, Object, Number, or String, but encountered Array"

        context "when the constraint is a Number" $ do
          it "accepts 1" $ do
            [yaml|
              hpack: 1
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==1" })]

          it "accepts 1.0" $ do
            [yaml|
              hpack: 1.0
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==1.0" })]

          it "accepts 0.11" $ do
            [yaml|
              hpack: 0.11
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==0.11" })]

          it "accepts 0.110" $ do
            [yaml|
              hpack: 0.110
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==0.110" })]

          it "accepts 1e2" $ do
            [yaml|
              hpack: 1e2
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==100" })]

        context "when the constraint is a String" $ do
          it "accepts version ranges" $ do
            [yaml|
              hpack: '>=2'
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange ">=2" })]

          it "accepts specific versions" $ do
            [yaml|
              hpack: 0.10.8.2
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==0.10.8.2" })]

          it "accepts wildcard versions" $ do
            [yaml|
              hpack: 2.*
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==2.*" })]

          it "accepts ^>=" $ do
            [yaml|
              hpack: ^>= 1.2.3.4
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange ">=1.2.3.4 && <1.3" })]

          it "reports parse errors" $ do
            [yaml|
              hpack: foo
            |] `shouldDecodeTo` left "Error while parsing $.hpack - invalid constraint \"foo\""

        context "when the constraint is an Object" $ do
          it "accepts explicit version field" $ do
            [yaml|
            hpack:
              version: 0.1.0
            |] `shouldDecodeTo_` Dependencies [("hpack", defaultInfo { dependencyInfoVersion = versionRange "==0.1.0" })]

          it "accepts github dependencies" $ do
            let source = Just (GitRef "https://github.com/haskell/cabal" "d53b6e0d908dfedfdf4337b2935519fb1d689e76" (Just "Cabal"))
            [yaml|
              Cabal:
                github: haskell/cabal
                ref: d53b6e0d908dfedfdf4337b2935519fb1d689e76
                subdir: Cabal
            |] `shouldDecodeTo_` Dependencies [("Cabal", defaultInfo { dependencyInfoVersion = DependencyVersion source AnyVersion })]

          it "ignores names in nested hashes" $ do
            let source = Just (Local "somewhere")
            [yaml|
              outer-name:
                name: inner-name
                path: somewhere
            |] `shouldDecodeTo` Right (Dependencies [("outer-name", defaultInfo { dependencyInfoVersion = DependencyVersion source AnyVersion })], ["$.outer-name.name"], [])

          it "defaults to any version" $ do
            [yaml|
              foo: {}
            |] `shouldDecodeTo_` Dependencies [("foo", defaultInfo)]

          context "with a version key" $ do
            it "rejects objects" $ do
              [yaml|
                foo:
                  version: {}
              |] `shouldDecodeTo` left "Error while parsing $.foo.version - expected Null, Number, or String, but encountered Object"

            it "accepts a string" $ do
              [yaml|
                foo:
                  version: ">= 3.2.5 && < 3.3"
              |] `shouldDecodeTo_` Dependencies [("foo", defaultInfo { dependencyInfoVersion = versionRange ">=3.2.5 && <3.3" })]

            it "accepts a specific version as a number" $ do
              [yaml|
                foo:
                  version: 3.0
              |] `shouldDecodeTo_` Dependencies [("foo", defaultInfo { dependencyInfoVersion = versionRange "==3.0" })]

            it "accepts a specific version as a string" $ do
              [yaml|
                foo:
                  version: 3.0.2
              |] `shouldDecodeTo_` Dependencies [("foo", defaultInfo { dependencyInfoVersion = versionRange "==3.0.2" })]

          context "with mixin" $ do
            it "accepts a single value" $ do
              [yaml|
                foo:
                  mixin: (Foo as Bar)
              |] `shouldDecodeTo_` Dependencies [("foo", defaultInfo { dependencyInfoMixins = ["(Foo as Bar)"] })]

            it "accepts a list" $ do
              [yaml|
                foo:
                  mixin:
                    - (Foo as Bar)
                    - hiding (Spam)
              |] `shouldDecodeTo_` Dependencies [("foo", defaultInfo { dependencyInfoMixins = ["(Foo as Bar)", "hiding (Spam)"] })]
