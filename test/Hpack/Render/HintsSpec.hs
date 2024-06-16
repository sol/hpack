module Hpack.Render.HintsSpec (spec) where

import           Test.Hspec

import           Hpack.Render.Hints
import           Hpack.Render.Dsl

spec :: Spec
spec = do
  describe "sniffRenderSettings" $ do
    context "when sniffed indentation is < default" $ do
      it "uses default instead" $ do
        let input = [
                "library"
              , "exposed-modules:"
              , "    Foo"
              ]
        sniffIndentation input `shouldBe` Just 0
        renderSettingsIndentation (sniffRenderSettings input) `shouldBe` 2

  describe "extractFieldOrder" $ do
    it "extracts field order hints" $ do
      let input = [
              "name:           hpack"
            , "version:        0.0.0"
            , "license:"
            , "license-file: "
            , "build-type:     Simple"
            ]
      extractFieldOrder input `shouldBe` [
              "name"
            , "version"
            , "license"
            , "license-file"
            , "build-type"
            ]

  describe "extractSectionsFieldOrder" $ do
    it "splits input into sections" $ do
      let input = [
              "name:           hpack"
            , "version:        0.0.0"
            , ""
            , "library"
            , "  foo: 23"
            , "  bar: 42"
            , ""
            , "executable foo"
            , "  bar: 23"
            , "  baz: 42"
            ]
      extractSectionsFieldOrder input `shouldBe` [("library", ["foo", "bar"]), ("executable foo", ["bar", "baz"])]

  describe "sanitize" $ do
    it "removes empty lines" $ do
      let input = [
              "foo"
            , ""
            , "   "
            , "  bar  "
            , "  baz"
            ]
      sanitize input `shouldBe` [
              "foo"
            , "  bar"
            , "  baz"
            ]

    it "removes trailing whitespace" $ do
      sanitize ["foo  ", "bar  "] `shouldBe` ["foo", "bar"]

    it "removes cabal-version" $ do
      sanitize ["cabal-version: 2.2", "bar  "] `shouldBe` ["bar"]

  describe "unindent" $ do
    it "unindents" $ do
      let input = [
              "   foo"
            , "  bar"
            , "   baz"
            ]
      unindent input `shouldBe` [
              " foo"
            , "bar"
            , " baz"
            ]

  describe "sniffAlignment" $ do
    it "sniffs field alignment from given cabal file" $ do
      let input = [
              "name:           hpack"
            , "version:        0.0.0"
            , "license:        MIT"
            , "license-file:   LICENSE"
            , "build-type:     Simple"
            ]
      sniffAlignment input `shouldBe` Just 16

    it "ignores fields without a value on the same line" $ do
      let input = [
              "name:           hpack"
            , "version:        0.0.0"
            , "description: "
            , "  foo"
            , "  bar"
            ]
      sniffAlignment input `shouldBe` Just 16

    context "when all fields are padded with exactly one space" $ do
      it "returns 0" $ do
        let input = [
                "name: hpack"
              , "version: 0.0.0"
              , "license: MIT"
              , "license-file: LICENSE"
              , "build-type: Simple"
              ]
        sniffAlignment input `shouldBe` Just 0

    context "with an empty input list" $ do
      it "returns Nothing" $ do
        let input = []
        sniffAlignment input `shouldBe` Nothing

  describe "splitField" $ do
    it "splits fields" $ do
      splitField "foo:   bar" `shouldBe` Just ("foo", "   bar")

    it "accepts fields names with dashes" $ do
      splitField "foo-bar: baz" `shouldBe` Just ("foo-bar", " baz")

    it "rejects fields names with spaces" $ do
      splitField "foo bar: baz" `shouldBe` Nothing

    it "rejects invalid fields" $ do
      splitField "foo bar" `shouldBe` Nothing

  describe "sniffIndentation" $ do
    it "sniffs indentation from executable section" $ do
      let input = [
              "name: foo"
            , "version: 0.0.0"
            , ""
            , "executable foo"
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

    it "sniffs indentation from library section" $ do
      let input = [
              "name: foo"
            , "version: 0.0.0"
            , ""
            , "library"
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

    it "ignores empty lines" $ do
      let input = [
              "executable foo"
            , ""
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

    it "ignores whitespace lines" $ do
      let input = [
              "executable foo"
            , "  "
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

  describe "sniffCommaStyle" $ do
    it "detects leading commas" $ do
      let input = [
              "executable foo"
            , "  build-depends:"
            , "      bar"
            , "    , baz"
            ]
      sniffCommaStyle input `shouldBe` Just LeadingCommas

    it "detects trailing commas" $ do
      let input = [
              "executable foo"
            , "  build-depends:"
            , "    bar,  "
            , "    baz"
            ]
      sniffCommaStyle input `shouldBe` Just TrailingCommas

    context "when detection fails" $ do
      it "returns Nothing" $ do
        sniffCommaStyle [] `shouldBe` Nothing
