{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Data.Aeson.Config.FromValueSpec where

import           Helper

import           GHC.Generics
import qualified Data.Map.Lazy as Map
import           Data.Monoid (Last(..))

import           Data.Aeson.Config.FromValue

shouldDecodeTo :: (HasCallStack, Eq a, Show a, FromValue a) => Value -> Result a -> Expectation
shouldDecodeTo value expected = decodeValue value `shouldBe` expected

shouldDecodeTo_ :: (HasCallStack, Eq a, Show a, FromValue a) => Value -> a -> Expectation
shouldDecodeTo_ value expected = decodeValue value `shouldBe` Right (expected, [], [])

data Person = Person {
  personName :: String
, personAge :: Int
, personAddress :: Maybe Address
} deriving (Eq, Show, Generic, FromValue)

data Address = Address {
  addressRegion :: String
, addressZip :: String
} deriving (Eq, Show, Generic, FromValue)

data Job = Job {
  jobRole :: String
, jobSalary :: Int
} deriving (Eq, Show, Generic, FromValue)

data FlatMaybe = FlatMaybe {
  flatMaybeValue :: Maybe String
} deriving (Eq, Show, Generic, FromValue)

data AliasMaybe = AliasMaybe {
  aliasMaybeValue :: Alias 'False "some-alias" (Maybe String)
} deriving (Eq, Show, Generic, FromValue)

data NestedMaybe = NestedMaybe {
  nestedMaybeValue :: Maybe (Maybe String)
} deriving (Eq, Show, Generic, FromValue)

data AliasNestedMaybe = AliasNestedMaybe {
  aliasNestedMaybeValue :: Alias 'False "some-alias" (Maybe (Maybe String))
} deriving (Eq, Show, Generic, FromValue)

data FlatLast = FlatLast {
  flatLastValue :: Last String
} deriving (Eq, Show, Generic, FromValue)

data AliasLast = AliasLast {
  aliasLastValue :: Alias 'False "some-alias" (Last String)
} deriving (Eq, Show, Generic, FromValue)

spec :: Spec
spec = do
  describe "fromValue" $ do
    context "with a record" $ do
      let
        left :: String -> Result Person
        left = Left
      it "decodes a record" $ do
        [yaml|
        name: "Joe"
        age: 23
        |] `shouldDecodeTo_` Person "Joe" 23 Nothing

      it "captures unrecognized fields" $ do
        [yaml|
        name: "Joe"
        age: 23
        foo: bar
        |] `shouldDecodeTo` Right (Person "Joe" 23 Nothing, ["$.foo"], [])

      it "captures nested unrecognized fields" $ do
        [yaml|
        name: "Joe"
        age: 23
        address:
          region: somewhere
          zip: "123456"
          foo:
            bar: 23
        |] `shouldDecodeTo` Right (Person "Joe" 23 (Just (Address "somewhere" "123456")), ["$.address.foo"], [])

      it "ignores fields that start with an underscore" $ do
        [yaml|
        name: "Joe"
        age: 23
        address:
          region: somewhere
          zip: "123456"
          _foo:
            bar: 23
        |] `shouldDecodeTo_` Person "Joe" 23 (Just (Address "somewhere" "123456"))

      it "fails on missing field" $ do
        [yaml|
        name: "Joe"
        |] `shouldDecodeTo` left "Error while parsing $ - key \"age\" not present"

      it "fails on invalid field value" $ do
        [yaml|
        name: "Joe"
        age: "23"
        |] `shouldDecodeTo` left "Error while parsing $.age - parsing Int failed, expected Number, but encountered String"

      context "when parsing a field of type (Maybe a)" $ do
        it "accepts a value" $ do
          [yaml|
          value: some value
          |] `shouldDecodeTo_` FlatMaybe (Just "some value")

        it "allows the field to be omitted" $ do
          [yaml|
          {}
          |] `shouldDecodeTo_` FlatMaybe Nothing

        it "rejects null" $ do
          [yaml|
          value: null
          |] `shouldDecodeTo` (Left "Error while parsing $.value - expected String, but encountered Null" :: Result FlatMaybe)

      context "when parsing a field of type (Maybe (Maybe a))" $ do
        it "accepts a value" $ do
          [yaml|
          value: some value
          |] `shouldDecodeTo_` NestedMaybe (Just $ Just "some value")

        it "allows the field to be omitted" $ do
          [yaml|
          {}
          |] `shouldDecodeTo_` NestedMaybe Nothing

        it "accepts null" $ do
          [yaml|
          value: null
          |] `shouldDecodeTo_` NestedMaybe (Just Nothing)

      context "when parsing a field of type (Alias (Maybe a))" $ do
        it "accepts a value" $ do
          [yaml|
          value: some value
          |] `shouldDecodeTo_` AliasMaybe (Alias $ Just "some value")

        it "allows the field to be accessed by its alias" $ do
          [yaml|
          some-alias: some alias value
          |] `shouldDecodeTo_` AliasMaybe (Alias $ Just "some alias value")

        it "gives the primary name precedence" $ do
          [yaml|
          value: some value
          some-alias: some alias value
          |] `shouldDecodeTo` Right (AliasMaybe (Alias $ Just "some value"), ["$.some-alias"], [])

        it "allows the field to be omitted" $ do
          [yaml|
          {}
          |] `shouldDecodeTo_` AliasMaybe (Alias Nothing)

        it "rejects null" $ do
          [yaml|
          value: null
          |] `shouldDecodeTo` (Left "Error while parsing $.value - expected String, but encountered Null" :: Result AliasMaybe)

      context "when parsing a field of type (Alias (Maybe (Maybe a)))" $ do
        it "accepts a value" $ do
          [yaml|
          value: some value
          |] `shouldDecodeTo_` AliasNestedMaybe (Alias . Just $ Just "some value")

        it "allows the field to be accessed by its alias" $ do
          [yaml|
          some-alias: some value
          |] `shouldDecodeTo_` AliasNestedMaybe (Alias . Just $ Just "some value")

        it "gives the primary name precedence" $ do
          [yaml|
          value: some value
          some-alias: some alias value
          |] `shouldDecodeTo` Right (AliasNestedMaybe (Alias . Just $ Just "some value"), ["$.some-alias"], [])

        it "allows the field to be omitted" $ do
          [yaml|
          {}
          |] `shouldDecodeTo_` AliasNestedMaybe (Alias Nothing)

        it "accepts null" $ do
          [yaml|
          value: null
          |] `shouldDecodeTo_` AliasNestedMaybe (Alias $ Just Nothing)

      context "when parsing a field of type (Last a)" $ do
        it "accepts a value" $ do
          [yaml|
          value: some value
          |] `shouldDecodeTo_` FlatLast (Last $ Just "some value")

        it "allows the field to be omitted" $ do
          [yaml|
          {}
          |] `shouldDecodeTo_` FlatLast (Last Nothing)

        it "rejects null" $ do
          [yaml|
          value: null
          |] `shouldDecodeTo` (Left "Error while parsing $.value - expected String, but encountered Null" :: Result FlatLast)

      context "when parsing a field of type (Alias (Last a))" $ do
        it "accepts a value" $ do
          [yaml|
          value: some value
          |] `shouldDecodeTo_` AliasLast (Alias . Last $ Just "some value")

        it "allows the field to be accessed by its alias" $ do
          [yaml|
          some-alias: some value
          |] `shouldDecodeTo_` AliasLast (Alias . Last $ Just "some value")

        it "gives the primary name precedence" $ do
          [yaml|
          value: some value
          some-alias: some alias value
          |] `shouldDecodeTo` Right (AliasLast (Alias . Last $ Just "some value"), ["$.some-alias"], [])

        it "allows the field to be omitted" $ do
          [yaml|
          {}
          |] `shouldDecodeTo_` AliasLast (Alias $ Last Nothing)

        it "rejects null" $ do
          [yaml|
          value: null
          |] `shouldDecodeTo` (Left "Error while parsing $.value - expected String, but encountered Null" :: Result AliasLast)

    context "with (,)" $ do
      it "captures unrecognized fields" $ do
        [yaml|
        name: Joe
        age: 23
        role: engineer
        salary: 100000
        foo: bar
        |] `shouldDecodeTo` Right ((Person "Joe" 23 Nothing, Job "engineer" 100000), ["$.foo"], [])

    context "with []" $ do
      it "captures unrecognized fields" $ do
        let
          expected = [Person "Joe" 23 (Just (Address "somewhere" "123456")), Person "Marry" 25 Nothing]
        [yaml|
        - name: "Joe"
          age: 23
          address:
            region: somewhere
            zip: "123456"
            foo: 23
        - name: "Marry"
          age: 25
          bar: 42
        |] `shouldDecodeTo` Right (expected, ["$[1].bar", "$[0].address.foo"], [])

    context "with Map" $ do
      it "captures unrecognized fields" $ do
        [yaml|
        Joe:
          region: somewhere
          zip: '123456'
          foo: bar
        |] `shouldDecodeTo` Right (Map.fromList [("Joe", Address "somewhere" "123456")], ["$.Joe.foo"], [])
