{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Hpack.Syntax.DependencySpec (spec) where

import           Helper

import           Data.Aeson.Config.FromValueSpec (shouldDecodeTo, shouldDecodeTo_)

import           Data.Aeson.Config.FromValue
import           Hpack.Syntax.Dependency

left :: String -> Result Dependencies
left = Left

spec :: Spec
spec = do
  describe "fromValue" $ do
    context "when parsing Dependencies" $ do
      context "with a scalar" $ do
        it "accepts dependencies without constraints" $ do
          [yaml|
            hpack
          |] `shouldDecodeTo_` Dependencies [("hpack", AnyVersion)]

        it "accepts dependencies with constraints" $ do
          [yaml|
            hpack >= 2 && < 3
          |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange ">=2 && <3")]

        context "with invalid constraint" $ do
          it "returns an error message" $ do
            [yaml|
              hpack ==
            |] `shouldDecodeTo` left "Error while parsing $ - invalid dependency \"hpack ==\""

      context "with a list" $ do
        it "accepts dependencies without constraints" $ do
          [yaml|
            - hpack
          |] `shouldDecodeTo_` Dependencies [("hpack", AnyVersion)]

        it "accepts dependencies with constraints" $ do
          [yaml|
            - hpack >= 2 && < 3
          |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange ">=2 && <3")]

        it "accepts git dependencies" $ do
          let source = GitRef "https://github.com/sol/hpack" "master" Nothing
          [yaml|
            - name: hpack
              git: https://github.com/sol/hpack
              ref: master
          |] `shouldDecodeTo_` Dependencies [("hpack", SourceDependency source)]

        it "accepts github dependencies" $ do
          let source = GitRef "https://github.com/sol/hpack" "master" Nothing
          [yaml|
            - name: hpack
              github: sol/hpack
              ref: master
          |] `shouldDecodeTo_` Dependencies [("hpack", SourceDependency source)]

        it "accepts an optional subdirectory for git dependencies" $ do
          let source = GitRef "https://github.com/yesodweb/wai" "master" (Just "warp")
          [yaml|
            - name: warp
              github: yesodweb/wai
              ref: master
              subdir: warp
          |] `shouldDecodeTo_` Dependencies [("warp", SourceDependency source)]

        it "accepts local dependencies" $ do
          let source = Local "../hpack"
          [yaml|
            - name: hpack
              path: ../hpack
          |] `shouldDecodeTo_` Dependencies [("hpack", SourceDependency source)]

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
          |] `shouldDecodeTo_` Dependencies [("array", AnyVersion)]

        it "rejects invalid values" $ do
          [yaml|
            hpack: []
          |] `shouldDecodeTo` left "Error while parsing $.hpack - expected Null, Object, Number, or String, encountered Array"

        context "when the constraint is a Number" $ do
          it "accepts 1" $ do
            [yaml|
              hpack: 1
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==1")]

          it "accepts 1.0" $ do
            [yaml|
              hpack: 1.0
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==1.0")]

          it "accepts 0.11" $ do
            [yaml|
              hpack: 0.11
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==0.11")]

          it "accepts 0.110" $ do
            [yaml|
              hpack: 0.110
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==0.110")]

          it "accepts 1e2" $ do
            [yaml|
              hpack: 1e2
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==100")]

        context "when the constraint is a String" $ do
          it "accepts version ranges" $ do
            [yaml|
              hpack: '>=2'
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange ">=2")]

          it "accepts specific versions" $ do
            [yaml|
              hpack: 0.10.8.2
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==0.10.8.2")]

          it "accepts wildcard versions" $ do
            [yaml|
              hpack: 2.*
            |] `shouldDecodeTo_` Dependencies [("hpack", VersionRange "==2.*")]

          it "reports parse errors" $ do
            [yaml|
              hpack: foo
            |] `shouldDecodeTo` left "Error while parsing $.hpack - invalid constraint \"foo\""

        context "when the constraint is an Object" $ do
          it "accepts github dependencies" $ do
            [yaml|
              Cabal:
                github: haskell/cabal
                ref: d53b6e0d908dfedfdf4337b2935519fb1d689e76
                subdir: Cabal
            |] `shouldDecodeTo_` Dependencies [("Cabal", SourceDependency (GitRef "https://github.com/haskell/cabal" "d53b6e0d908dfedfdf4337b2935519fb1d689e76" (Just "Cabal")))]

          it "ignores names in nested hashes" $ do
            [yaml|
              outer-name:
                name: inner-name
                path: somewhere
            |] `shouldDecodeTo` Right (Dependencies [("outer-name", SourceDependency (Local "somewhere"))], ["$.outer-name.name"])
