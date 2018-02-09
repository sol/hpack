{-# LANGUAGE QuasiQuotes #-}
module Hpack.Syntax.DefaultsSpec (spec) where

import           Helper

import           Data.Aeson.Config.FromValueSpec hiding (spec)

import           Data.Aeson.Config.FromValue
import           Hpack.Syntax.Defaults

defaultsGithub :: String -> String -> String -> [FilePath] -> Defaults
defaultsGithub owner repo ref path = DefaultsGithub $ Github owner repo ref path

spec :: Spec
spec = do
  describe "isValidOwner" $ do
    it "rejects the empty string" $ do
      isValidOwner "" `shouldBe` False

    it "accepts valid owner names" $ do
      isValidOwner "Foo-Bar-23" `shouldBe` True

    it "rejects dots" $ do
      isValidOwner "foo.bar" `shouldBe` False

    it "rejects multiple consecutive hyphens" $ do
      isValidOwner "foo--bar" `shouldBe` False

    it "rejects hyphens at the beginning" $ do
      isValidOwner "-foo" `shouldBe` False

    it "rejects hyphens at the end" $ do
      isValidOwner "foo-" `shouldBe` False

  describe "isValidRepo" $ do
    it "rejects the empty string" $ do
      isValidRepo "" `shouldBe` False

    it "rejects ." $ do
      isValidRepo "." `shouldBe` False

    it "rejects .." $ do
      isValidRepo ".." `shouldBe` False

    it "accepts underscores" $ do
      isValidRepo "foo_bar" `shouldBe` True

    it "accepts dots" $ do
      isValidRepo "foo.bar" `shouldBe` True

    it "accepts hyphens" $ do
      isValidRepo "foo-bar" `shouldBe` True

  describe "fromValue" $ do
    context "when parsing Defaults" $ do
      let
        left :: String -> DecodeResult Defaults
        left = Left
      context "with Object" $ do
        it "fails when neither github nor local is present" $ do
          [yaml|
          defaults:
            foo: one
            bar: two
          library: {}
          |] `shouldDecodeTo` left "Error while parsing $ - neither key \"github\" nor key \"local\" present"

        it "accepts Defaults from GitHub" $ do
          [yaml|
          github: sol/hpack
          ref: 0.1.0
          path: defaults.yaml
          |] `shouldDecodeTo_` defaultsGithub "sol" "hpack" "0.1.0" ["defaults.yaml"]

        it "rejects invalid owner names" $ do
          [yaml|
          github: ../hpack
          ref: 0.1.0
          path: defaults.yaml
          |] `shouldDecodeTo` left "Error while parsing $.github - invalid owner name \"..\""

        it "rejects invalid repository names" $ do
          [yaml|
          github: sol/..
          ref: 0.1.0
          path: defaults.yaml
          |] `shouldDecodeTo` left "Error while parsing $.github - invalid repository name \"..\""

        it "rejects invalid Git references" $ do
          [yaml|
          github: sol/hpack
          ref: ../foo/bar
          path: defaults.yaml
          |] `shouldDecodeTo` left "Error while parsing $.ref - invalid Git reference \"../foo/bar\""

        it "rejects \\ in path" $ do
          [yaml|
          github: sol/hpack
          ref: 0.1.0
          path: hpack\defaults.yaml
          |] `shouldDecodeTo` left "Error while parsing $.path - rejecting '\\' in \"hpack\\\\defaults.yaml\", please use '/' to separate path components"

        it "rejects : in path" $ do
          [yaml|
          github: sol/hpack
          ref: 0.1.0
          path: foo:bar.yaml
          |] `shouldDecodeTo` left "Error while parsing $.path - rejecting ':' in \"foo:bar.yaml\""

        it "rejects absolute paths" $ do
          [yaml|
          github: sol/hpack
          ref: 0.1.0
          path: /defaults.yaml
          |] `shouldDecodeTo` left "Error while parsing $.path - rejecting absolute path \"/defaults.yaml\""

        it "rejects .. in path" $ do
          [yaml|
          github: sol/hpack
          ref: 0.1.0
          path: ../../defaults.yaml
          |] `shouldDecodeTo` left "Error while parsing $.path - rejecting \"..\" in \"../../defaults.yaml\""

      context "with String" $ do
        it "accepts Defaults from GitHub" $ do
          [yaml|
          sol/hpack@0.1.0
          |] `shouldDecodeTo_` defaultsGithub "sol" "hpack" "0.1.0" [".hpack", "defaults.yaml"]

        it "rejects invalid owner names" $ do
          [yaml|
          ../hpack@0.1.0
          |] `shouldDecodeTo` left "Error while parsing $ - invalid owner name \"..\""

        it "rejects invalid repository names" $ do
          [yaml|
          sol/..@0.1.0
          |] `shouldDecodeTo` left "Error while parsing $ - invalid repository name \"..\""

        it "rejects invalid Git references" $ do
          [yaml|
          sol/pack@../foo/bar
          |] `shouldDecodeTo` left "Error while parsing $ - invalid Git reference \"../foo/bar\""

        it "rejects missing Git reference" $ do
          [yaml|
          sol/hpack
          |] `shouldDecodeTo` left "Error while parsing $ - missing Git reference for \"sol/hpack\", the expected format is owner/repo@ref"

      context "with neither Object nor String" $ do
        it "fails" $ do
          [yaml|
          10
          |] `shouldDecodeTo` left "Error while parsing $ - expected Object or String, encountered Number"
