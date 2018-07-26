{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
module Hpack.Syntax.BuildToolsSpec (spec) where

import           Helper

import           Data.Aeson.Config.FromValueSpec (shouldDecodeTo_)

import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.BuildTools

spec :: Spec
spec = do
  describe "fromValue" $ do
    context "when parsing BuildTools" $ do
      context "with a scalar" $ do
        it "accepts qualified names" $ do
          [yaml|
            foo:bar
          |] `shouldDecodeTo_` BuildTools [(BuildTool "foo" "bar", AnyVersion)]

        it "accepts qualified names with a version" $ do
          [yaml|
            foo:bar >= 0.1.0
          |] `shouldDecodeTo_` BuildTools [(BuildTool "foo" "bar", VersionRange ">=0.1.0")]

        it "accepts unqualified names" $ do
          [yaml|
            foo
          |] `shouldDecodeTo_` BuildTools [(BuildTool "foo" "foo", AnyVersion)]

        it "accepts unqualified names with a version" $ do
          [yaml|
            foo >= 0.1.0
          |] `shouldDecodeTo_` BuildTools [(BuildTool "foo" "foo", VersionRange ">=0.1.0")]

      context "with a mapping" $ do
        it "accepts qualified names" $ do
          [yaml|
            foo:bar: 0.1.0
          |] `shouldDecodeTo_` BuildTools [(BuildTool "foo" "bar", VersionRange "==0.1.0")]

        it "accepts unqualified names" $ do
          [yaml|
            foo: 0.1.0
          |] `shouldDecodeTo_` BuildTools [(BuildTool "foo" "foo", VersionRange "==0.1.0")]

      context "with a list" $ do
        it "accepts a list of build tools" $ do
          [yaml|
            - foo:one
            - bar:two >= 0.1.0
            - baz == 0.2.0
          |] `shouldDecodeTo_` BuildTools [
              (BuildTool "foo" "one", AnyVersion)
            , (BuildTool "bar" "two", VersionRange ">=0.1.0")
            , (BuildTool "baz" "baz", VersionRange "==0.2.0")
            ]

        it "accepts source dependencies with a qualified name" $ do
          let source = GitRef "https://github.com/sol/hpack" "master" Nothing
          [yaml|
            - name: hpack:foo
              github: sol/hpack
              ref: master
          |] `shouldDecodeTo_` BuildTools [(BuildTool "hpack" "foo", SourceDependency source)]

        it "accepts source dependencies with an unqualified name" $ do
          let source = GitRef "https://github.com/sol/hpack" "master" Nothing
          [yaml|
            - name: hpack
              github: sol/hpack
              ref: master
          |] `shouldDecodeTo_` BuildTools [(BuildTool "hpack" "hpack", SourceDependency source)]
