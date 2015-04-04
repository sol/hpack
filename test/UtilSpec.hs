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
