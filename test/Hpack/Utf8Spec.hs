{-# LANGUAGE OverloadedStrings #-}
module Hpack.Utf8Spec (spec) where

import           Helper

import qualified Data.ByteString as B
import           System.Directory

import qualified Hpack.Utf8 as Utf8

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "readFile" $ do
    context "with a file that uses CRLF newlines" $ do
      it "applies newline conversion" $ do
        let
          name = "foo.txt"
        B.writeFile name "foo\r\nbar"
        Utf8.readFile name `shouldReturn` "foo\nbar"

  describe "writeFile" $ do
    it "uses system specific newline encoding" $ do
      let
        name = "foo.txt"
        c = "foo\nbar"

      writeFile name c
      systemSpecific <- B.readFile name

      Utf8.writeFile name c
      B.readFile name `shouldReturn` systemSpecific

  describe "ensureFile" $ do
    it "creates a file" $ do
      Utf8.ensureFile "foo" "bar"
      readFile "foo" `shouldReturn` "bar"

    it "does not unnecessarily touch a file" $ do
      Utf8.ensureFile "foo" "bar"
      let t = read "2020-02-28 23:23:23 UTC"
      setModificationTime "foo" t
      Utf8.ensureFile "foo" "bar"
      getModificationTime "foo" `shouldReturn` t

    it "creates directories as needed" $ do
      Utf8.ensureFile "foo/bar/baz" "23"
      readFile "foo/bar/baz" `shouldReturn` "23"
