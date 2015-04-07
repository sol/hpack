{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (main, spec) where

import           Test.Hspec
import           Data.Aeson

import           Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toModule" $ do
    it "maps a path to a module name" $ do
      toModule "Foo/Bar/Baz.hs" `shouldBe` Just "Foo.Bar.Baz"

  describe "List" $ do
    it "can be a single value" $ do
      fromJSON (toJSON $ Number 23) `shouldBe` Success (List [23 :: Int])

    it "can be a list of values" $ do
      fromJSON (toJSON [Number 23, Number 42]) `shouldBe` Success (List [23, 42 :: Int])

  describe "tryReadFile" $ do
    it "reads file" $ do
      tryReadFile "test/asset/foo" `shouldReturn` Just "foo\n"

    it "returns Nothing if file does not exist" $ do
      tryReadFile "test/asset/bar" `shouldReturn` Nothing

  describe "sniffAlignment" $ do
    it "sniffs field alignment from given cabal file" $ do
      let input = unlines [
              "name:           cabalize"
            , "version:        0.0.0"
            , "license:        MIT"
            , "license-file:   LICENSE"
            , "build-type:     Simple"
            , "cabal-version:  >= 1.10"
            ]
      sniffAlignment input `shouldBe` Just 16

  describe "splitField" $ do
    it "splits fields" $ do
      splitField "foo:   bar" `shouldBe` Just ("foo:", "   bar")

    it "rejects fields without a value" $ do
      splitField "foo:" `shouldBe` Nothing
