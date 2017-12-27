{-# LANGUAGE QuasiQuotes #-}
module Hpack.SyntaxSpec (spec) where

import           Helper
import           Data.String.Interpolate.IsString

import           Hpack.Syntax

spec :: Spec
spec = do
  describe "isValidUser" $ do
    it "rejects the empty string" $ do
      isValidUser "" `shouldBe` False

    it "accepts valid user names" $ do
      isValidUser "Foo-Bar-23" `shouldBe` True

    it "rejects dots" $ do
      isValidUser "foo.bar" `shouldBe` False

    it "rejects multiple consecutive hyphens" $ do
      isValidUser "foo--bar" `shouldBe` False

    it "rejects hyphens at the beginning" $ do
      isValidUser "-foo" `shouldBe` False

    it "rejects hyphens at the end" $ do
      isValidUser "foo-" `shouldBe` False

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

  describe "parseJSON" $ do
    context "when parsing Defaults" $ do
      context "with Object" $ do
        it "accepts Defaults from GitHub" $ do
          [i|
          github: sol/hpack
          ref: 0.1.0
          path: defaults.yaml
          |] `shouldParseAs` Right Defaults {
              defaultsGithubUser = "sol"
            , defaultsGithubRepo = "hpack"
            , defaultsRef = "0.1.0"
            , defaultsPath = ["defaults.yaml"]
            }

        it "rejects invalid user names" $ do
          [i|
          github: ../hpack
          ref: 0.1.0
          path: defaults.yaml
          |] `shouldParseAs` (Left "Error in $.github: invalid user name \"..\"" :: Either String Defaults)

        it "rejects invalid repository names" $ do
          [i|
          github: sol/..
          ref: 0.1.0
          path: defaults.yaml
          |] `shouldParseAs` (Left "Error in $.github: invalid repository name \"..\"" :: Either String Defaults)

        it "rejects invalid Git references" $ do
          [i|
          github: sol/hpack
          ref: ../foo/bar
          path: defaults.yaml
          |] `shouldParseAs` (Left "Error in $.ref: invalid Git reference \"../foo/bar\"" :: Either String Defaults)

        it "rejects \\ in path" $ do
          [i|
          github: sol/hpack
          ref: 0.1.0
          path: hpack\\defaults.yaml
          |] `shouldParseAs` (Left "Error in $.path: rejecting '\\' in \"hpack\\\\defaults.yaml\", please use '/' to separate path components" :: Either String Defaults)

        it "rejects : in path" $ do
          [i|
          github: sol/hpack
          ref: 0.1.0
          path: foo:bar.yaml
          |] `shouldParseAs` (Left "Error in $.path: rejecting ':' in \"foo:bar.yaml\"" :: Either String Defaults)

        it "rejects absolute paths" $ do
          [i|
          github: sol/hpack
          ref: 0.1.0
          path: /defaults.yaml
          |] `shouldParseAs` (Left "Error in $.path: rejecting absolute path \"/defaults.yaml\"" :: Either String Defaults)

        it "rejects .. in path" $ do
          [i|
          github: sol/hpack
          ref: 0.1.0
          path: ../../defaults.yaml
          |] `shouldParseAs` (Left "Error in $.path: rejecting \"..\" in \"../../defaults.yaml\"" :: Either String Defaults)

      context "with String" $ do
        it "accepts Defaults from GitHub" $ do
          [i|
          sol/hpack@0.1.0
          |] `shouldParseAs` Right Defaults {
              defaultsGithubUser = "sol"
            , defaultsGithubRepo = "hpack"
            , defaultsRef = "0.1.0"
            , defaultsPath = [".hpack", "defaults.yaml"]
            }

        it "rejects invalid user names" $ do
          [i|
          ../hpack@0.1.0
          |] `shouldParseAs` (Left "Error in $: invalid user name \"..\"" :: Either String Defaults)

        it "rejects invalid repository names" $ do
          [i|
          sol/..@0.1.0
          |] `shouldParseAs` (Left "Error in $: invalid repository name \"..\"" :: Either String Defaults)

        it "rejects invalid Git references" $ do
          [i|
          sol/pack@../foo/bar
          |] `shouldParseAs` (Left "Error in $: invalid Git reference \"../foo/bar\"" :: Either String Defaults)

        it "rejects missing Git reference" $ do
          [i|
          sol/hpack
          |] `shouldParseAs` (Left "Error in $: missing Git reference for \"sol/hpack\", the expected format is user/repo@ref" :: Either String Defaults)

      context "with neither Object nor String" $ do
        it "fails" $ do
          [i|
          10
          |] `shouldParseAs` (Left "Error in $: expected Object or String, encountered Number" :: Either String Defaults)
