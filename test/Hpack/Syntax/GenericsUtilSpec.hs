{-# LANGUAGE DeriveGeneric #-}
module Hpack.Syntax.GenericsUtilSpec (spec) where

import           Test.Hspec

import           Data.Proxy
import           GHC.Generics

import           Hpack.Syntax.GenericsUtil

data Person = Person {
  _personName :: String
, _personAge :: Int
} deriving Generic

spec :: Spec
spec = do
  describe "selectors" $ do
    it "returns a list of record selectors" $ do
      selectors (Proxy :: Proxy Person) `shouldBe` ["_personName", "_personAge"]

  describe "typeName" $ do
    it "gets datatype name" $ do
      typeName (Proxy :: Proxy Person) `shouldBe` "Person"
