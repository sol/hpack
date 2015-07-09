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
        render 0 stanza `shouldBe` [
            "foo"
          , "  bar: 23"
          , "  baz: 42"
          ]
      it "omits empty fields" $ do
        let stanza = Stanza "foo" [
                Field "bar" "23"
              , Field "baz" (WordList [])
              ]
        render 0 stanza `shouldBe` [
            "foo"
          , "  bar: 23"
          ]

    context "when rendering a Field" $ do
      context "when rendering a MultipleLines value" $ do
        it "takes nesting into account" $ do
          let field = Field "foo" (CommaSeparatedList ["bar", "baz"])
          render 1 field `shouldBe` [
              "  foo:"
            , "      bar"
            , "    , baz"
            ]

        context "when value is empty" $ do
          it "returns an empty list" $ do
            let field = Field "foo" (CommaSeparatedList [])
            render 0 field `shouldBe` []

      context "when rendering a SingleLine value" $ do
        it "returns a single line" $ do
          let field = Field "foo" (Literal "bar")
          render 0 field `shouldBe` ["foo: bar"]

        it "takes nesting into account" $ do
          let field = Field "foo" (Literal "bar")
          render 2 field `shouldBe` ["    foo: bar"]

        context "when value is empty" $ do
          it "returns an empty list" $ do
            let field = Field "foo" (Literal "")
            render 0 field `shouldBe` []

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
