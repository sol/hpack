{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Aeson.Config.FromValueSpec where

import           Helper

import           GHC.Generics
import qualified Data.Map.Lazy as Map

import           Data.Aeson.Config.FromValue

shouldDecodeTo :: (HasCallStack, Eq a, Show a, FromValue a) => Value -> Result a -> Expectation
shouldDecodeTo value expected = decodeValue value `shouldBe` expected

shouldDecodeTo_ :: (HasCallStack, Eq a, Show a, FromValue a) => Value -> a -> Expectation
shouldDecodeTo_ value expected = decodeValue value `shouldBe` Right (expected, [])

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
        |] `shouldDecodeTo` Right (Person "Joe" 23 Nothing, [unknownField "$.foo"])

      it "captures nested unrecognized fields" $ do
        [yaml|
        name: "Joe"
        age: 23
        address:
          region: somewhere
          zip: "123456"
          foo:
            bar: 23
        |] `shouldDecodeTo` Right (Person "Joe" 23 (Just (Address "somewhere" "123456")), [unknownField "$.address.foo"])

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

    context "with (,)" $ do
      it "captures unrecognized fields" $ do
        [yaml|
        name: Joe
        age: 23
        role: engineer
        salary: 100000
        foo: bar
        |] `shouldDecodeTo` Right ((Person "Joe" 23 Nothing, Job "engineer" 100000), [unknownField "$.foo"])

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
        |] `shouldDecodeTo` Right (expected, [unknownField "$[1].bar", unknownField "$[0].address.foo"])

    context "with Map" $ do
      it "captures unrecognized fields" $ do
        [yaml|
        Joe:
          region: somewhere
          zip: '123456'
          foo: bar
        |] `shouldDecodeTo` Right (Map.fromList [("Joe", Address "somewhere" "123456")], [unknownField "$.Joe.foo"])
