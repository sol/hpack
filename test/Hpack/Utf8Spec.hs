{-# LANGUAGE OverloadedStrings #-}
module Hpack.Utf8Spec (spec) where

import           Helper

import qualified Data.ByteString as B

import qualified Hpack.Utf8 as Utf8

spec :: Spec
spec = do
  describe "readFile" $ do
    context "with a file that uses CRLF newlines" $ do
      it "applies newline conversion" $ do
        inTempDirectory $ do
          let
            name = "foo.txt"
          B.writeFile name "foo\r\nbar"
          Utf8.readFile name `shouldReturn` "foo\nbar"

  describe "ensureFile" $ do
    it "uses system specific newline encoding" $ do
      inTempDirectory $ do
        let
          name = "foo.txt"
          c = "foo\nbar"

        writeFile name c
        systemSpecific <- B.readFile name

        Utf8.ensureFile name c
        B.readFile name `shouldReturn` systemSpecific
