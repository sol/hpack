{-# LANGUAGE OverloadedStrings #-}
module Hpack.RenderSpec where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.QuickCheck
import           Data.List.Compat
import           Data.Maybe

import           Hpack.Render

spec :: Spec
spec = do
  describe "render" $ do
    let render_ = render defaultRenderSettings 0
    context "when rendering a Stanza" $ do
      it "renders stanza" $ do
        let stanza = Stanza "foo" [
                Field "bar" "23"
              , Field "baz" "42"
              ]
        render_ stanza `shouldBe` [
            "foo"
          , "  bar: 23"
          , "  baz: 42"
          ]

      it "omits empty fields" $ do
        let stanza = Stanza "foo" [
                Field "bar" "23"
              , Field "baz" (WordList [])
              ]
        render_ stanza `shouldBe` [
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

      it "renders nested stanzas" $ do
        let input = Stanza "foo" [Field "bar" "23", Stanza "baz" [Field "qux" "42"]]
        render_ input `shouldBe` [
            "foo"
          , "  bar: 23"
          , "  baz"
          , "    qux: 42"
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
            render_ field `shouldBe` []

      context "when rendering a SingleLine value" $ do
        it "returns a single line" $ do
          let field = Field "foo" (Literal "bar")
          render_ field `shouldBe` ["foo: bar"]

        it "takes nesting into account" $ do
          let field = Field "foo" (Literal "bar")
          render defaultRenderSettings 2 field `shouldBe` ["    foo: bar"]

        it "takes alignment into account" $ do
          let field = Field "foo" (Literal "bar")
          render defaultRenderSettings {renderSettingsFieldAlignment = 10} 0 field `shouldBe` ["foo:      bar"]

        context "when value is empty" $ do
          it "returns an empty list" $ do
            let field = Field "foo" (Literal "")
            render_ field `shouldBe` []

  describe "renderValue" $ do
    it "renders WordList" $ do
      renderValue defaultRenderSettings (WordList ["foo", "bar", "baz"]) `shouldBe` SingleLine "foo bar baz"

    it "renders CommaSeparatedList" $ do
      renderValue defaultRenderSettings (CommaSeparatedList ["foo", "bar", "baz"]) `shouldBe` MultipleLines [
          "  foo"
        , ", bar"
        , ", baz"
        ]

    it "renders LineSeparatedList" $ do
      renderValue defaultRenderSettings (LineSeparatedList ["foo", "bar", "baz"]) `shouldBe` MultipleLines [
          "  foo"
        , "  bar"
        , "  baz"
        ]

    context "when renderSettingsCommaStyle is TrailingCommas" $ do
      let settings = defaultRenderSettings{renderSettingsCommaStyle = TrailingCommas}

      it "renders CommaSeparatedList with trailing commas" $ do
        renderValue settings (CommaSeparatedList ["foo", "bar", "baz"]) `shouldBe` MultipleLines [
            "foo,"
          , "bar,"
          , "baz"
          ]

      it "renders LineSeparatedList without padding" $ do
        renderValue settings (LineSeparatedList ["foo", "bar", "baz"]) `shouldBe` MultipleLines [
            "foo"
          , "bar"
          , "baz"
          ]

  describe "sortFieldsBy" $ do
    let
      field name = Field name (Literal $ name ++ " value")
      arbitraryFieldNames = sublistOf ["foo", "bar", "baz", "qux", "foobar", "foobaz"] >>= shuffle

    it "sorts fields" $ do
      let fields = map field ["baz", "bar", "foo"]
      sortFieldsBy ["foo", "bar", "baz"] fields `shouldBe` map field ["foo", "bar", "baz"]

    it "keeps existing field order" $ do
      forAll (map field <$> arbitraryFieldNames) $ \fields -> do
        forAll arbitraryFieldNames $ \existingFieldOrder -> do
          let
            existingIndex :: Element -> Maybe Int
            existingIndex (Field name _) = name `elemIndex` existingFieldOrder
            existingIndex _ = Nothing

            indexes :: [Int]
            indexes = mapMaybe existingIndex (sortFieldsBy existingFieldOrder fields)

          sort indexes `shouldBe` indexes

    it "is stable" $ do
      forAll arbitraryFieldNames $ \fieldNames -> do
        forAll (elements $ subsequences fieldNames) $ \existingFieldOrder -> do
          let fields = map field fieldNames
          sortFieldsBy existingFieldOrder fields `shouldBe` fields

  describe "addSortKey" $ do
    it "adds sort key"  $ do
      addSortKey [(Nothing, "foo"), (Just 3, "bar"), (Nothing, "baz")] `shouldBe` [((-1, 0), "foo"), ((3, 1), "bar"), ((3, 2), "baz" :: String)]
