{-# LANGUAGE QuasiQuotes #-}
module Data.Aeson.Config.TypesSpec (spec) where

import           Helper
import           Data.Aeson.Config.FromValueSpec (shouldDecodeTo, shouldDecodeTo_)

import           Data.Aeson.Config.FromValue
import           Data.Aeson.Config.Types

spec :: Spec
spec = do
  describe "fromValue" $ do
    context "List" $ do
      let
        parseError :: String -> DecodeResult (List Int)
        parseError prefix = Left (prefix ++ " - expected Int, encountered String")

      context "when parsing single values" $ do
        it "returns the value in a singleton list" $ do
          [yaml|23|] `shouldDecodeTo_` (List [23 :: Int])

        it "returns error messages from element parsing" $ do
          [yaml|foo|] `shouldDecodeTo` parseError "Error while parsing $"

      context "when parsing a list of values" $ do
        it "returns the list" $ do
          [yaml|
          - 23
          - 42
          |] `shouldDecodeTo_` List [23, 42 :: Int]

        it "propagates parse error messages of invalid elements" $ do
          [yaml|
          - 23
          - foo
          |] `shouldDecodeTo` parseError "Error while parsing $[1]"
