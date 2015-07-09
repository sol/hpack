{-# LANGUAGE OverloadedStrings #-}

module Hpack.RenderSpec where

import Test.Hspec

import Hpack.Render

spec :: Spec
spec = do
  describe "render" $ do
    context "when rendering a Stanza" $ do
      it "renders stanza" $ do
        let stanza = Stanza "foo" [
                Field "bar" "23"
              , Field "baz" "42"
              ]
        render defaultRenderSettings 0 stanza `shouldBe` [
            "foo"
          , "  bar: 23"
          , "  baz: 42"
          ]

      it "omits empty fields" $ do
        let stanza = Stanza "foo" [
                Field "bar" "23"
              , Field "baz" (WordList [])
              ]
        render defaultRenderSettings 0 stanza `shouldBe` [
            "foo"
          , "  bar: 23"
          ]

      it "allows to customize indentation" $ do
        let stanza = Stanza "foo" [
                Field "bar" "23"
              , Field "baz" "42"
              ]
        render defaultRenderSettings{renderSettingsIndentation = 4} 0 stanza `shouldBe` [
            "foo"
          , "    bar: 23"
          , "    baz: 42"
          ]

    context "when rendering a Field" $ do
      context "when rendering a MultipleLines value" $ do
        it "takes nesting into account" $ do
          let field = Field "foo" (CommaSeparatedList ["bar", "baz"])
          render defaultRenderSettings 1 field `shouldBe` [
              "  foo:"
            , "      bar"
            , "    , baz"
            ]

        context "when value is empty" $ do
          it "returns an empty list" $ do
            let field = Field "foo" (CommaSeparatedList [])
            render defaultRenderSettings 0 field `shouldBe` []

      context "when rendering a SingleLine value" $ do
        it "returns a single line" $ do
          let field = Field "foo" (Literal "bar")
          render defaultRenderSettings 0 field `shouldBe` ["foo: bar"]

        it "takes nesting into account" $ do
          let field = Field "foo" (Literal "bar")
          render defaultRenderSettings 2 field `shouldBe` ["    foo: bar"]

        context "when value is empty" $ do
          it "returns an empty list" $ do
            let field = Field "foo" (Literal "")
            render defaultRenderSettings 0 field `shouldBe` []

  describe "renderValue" $ do
    it "renders WordList" $ do
      renderValue (WordList ["foo", "bar", "baz"]) `shouldBe` SingleLine "foo bar baz"

    it "renders CommaSeparatedList" $ do
      renderValue (CommaSeparatedList ["foo", "bar", "baz"]) `shouldBe` MultipleLines [
          "  foo"
        , ", bar"
        , ", baz"
        ]

    it "renders LineSeparatedList" $ do
      renderValue (LineSeparatedList ["foo", "bar", "baz"]) `shouldBe` MultipleLines [
          "  foo"
        , "  bar"
        , "  baz"
        ]

  describe "sniffIndentation" $ do
    it "sniff alignment from executable section" $ do
      let input = unlines [
              "name: foo"
            , "version: 0.0.0"
            , ""
            , "executable foo"
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

    it "sniff alignment from library section" $ do
      let input = unlines [
              "name: foo"
            , "version: 0.0.0"
            , ""
            , "library"
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

    it "ignores empty lines" $ do
      let input = unlines [
              "executable foo"
            , ""
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4

    it "ignores whitespace lines" $ do
      let input = unlines [
              "executable foo"
            , "  "
            , "    build-depends: bar"
            ]
      sniffIndentation input `shouldBe` Just 4
