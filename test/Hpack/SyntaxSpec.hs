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
      it "accepts Defaults from GitHub" $ do
        [i|
        github: sol/hpack
        ref: 0.1.0
        path: defaults.yaml
        |] `shouldParseAs` Right Defaults {
            defaultsGithubUser = "sol"
          , defaultsGithubRepo = "hpack"
          , defaultsRef = "0.1.0"
          , defaultsPath = "defaults.yaml"
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
        |] `shouldParseAs` (Left "Error in $.ref: invalid reference \"../foo/bar\"" :: Either String Defaults)
