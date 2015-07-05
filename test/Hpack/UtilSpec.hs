{-# LANGUAGE OverloadedStrings #-}
module Hpack.UtilSpec (main, spec) where

import           Helper
import           Data.Aeson
import           System.Directory

import           Hpack.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toModule" $ do
    it "maps paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.hs"] `shouldBe` Just "Foo.Bar.Baz"

    it "rejects invalid module names" $ do
      toModule ["resources", "hello.hs"] `shouldBe` Nothing

  describe "getFilesRecursive" $ do
    it "gets all files from given directory and all its subdirectories" $ do
      inTempDirectoryNamed "test" $ do
        touch "foo/bar"
        touch "foo/baz"
        touch "foo/foobar/baz"
        getFilesRecursive "foo" `shouldReturn` [
            ["bar"]
          , ["baz"]
          , ["foobar", "baz"]
          ]

  describe "List" $ do
    it "can be a single value" $ do
      fromJSON (toJSON $ Number 23) `shouldBe` Success (List [23 :: Int])

    it "can be a list of values" $ do
      fromJSON (toJSON [Number 23, Number 42]) `shouldBe` Success (List [23, 42 :: Int])

  describe "tryReadFile" $ do
    it "reads file" $ do
      inTempDirectory $ do
        writeFile "foo" "bar"
        tryReadFile "foo" `shouldReturn` Just "bar"

    it "returns Nothing if file does not exist" $ do
      inTempDirectory $ do
        tryReadFile "foo" `shouldReturn` Nothing

  describe "extractFieldOrderHint" $ do
    it "extracts field order hints" $ do
      let input = unlines [
              "name:           cabalize"
            , "version:        0.0.0"
            , "license:"
            , "license-file: "
            , "build-type:     Simple"
            , "cabal-version:  >= 1.10"
            ]
      extractFieldOrderHint input `shouldBe` [
              "name"
            , "version"
            , "license"
            , "license-file"
            , "build-type"
            , "cabal-version"
            ]

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

    it "ignores fields without a value on the same line" $ do
      let input = unlines [
              "name:           cabalize"
            , "version:        0.0.0"
            , "description: "
            , "  foo"
            , "  bar"
            ]
      sniffAlignment input `shouldBe` Just 16

  describe "splitField" $ do
    it "splits fields" $ do
      splitField "foo:   bar" `shouldBe` Just ("foo", "   bar")

    it "accepts fields names with dashes" $ do
      splitField "foo-bar: baz" `shouldBe` Just ("foo-bar", " baz")

    it "rejects fields names with spaces" $ do
      splitField "foo bar: baz" `shouldBe` Nothing

    it "rejects invalid fields" $ do
      splitField "foo bar" `shouldBe` Nothing

  describe "expandGlobs" $ around_ inTempDirectory $ before_ (touch "package.yaml") $ do
    it "accepts file globs in extra-source-files" $ do
      let files = map ("res/" ++) ["foo.bar", "hello", "world"]
      mapM_ touch files
      expandGlobs ["res/*"] `shouldReturn` ([], files)

    it "disallows duplicates in extra-source-files in presence of globs" $ do
      let file = "res/hello"
      touch file
      expandGlobs ["res/*", "res/hello"] `shouldReturn` ([], [file])

    it "expands globs followed by extension" $ do
      let file = "foo.js"
      touch file
      expandGlobs ["*.js"] `shouldReturn` ([], [file])

    it "expands directory globs" $ do
      touch "res/foo/hello.foo"
      touch "res/bar/hello.bar"
      expandGlobs ["res/*/*"] `shouldReturn` ([], ["res/bar/hello.bar", "res/foo/hello.foo"])

    it "expands ** globs" $ do
      let files = ["res/bar/hello.testfile", "res/foo/hello.testfile"]
      mapM_ touch files
      expandGlobs ["**/*.testfile"] `shouldReturn` ([], files)

    it "doesn't expand globs for directories" $ do
      touch "res/foo"
      createDirectory "res/testdirectory"
      expandGlobs ["res/**"] `shouldReturn` ([], ["res/foo"])

    it "doesn't preserve extra-source-files patterns which don't exist" $ do
      expandGlobs ["missing.foo", "res/*"] `shouldReturn` ([
          "Specified extra-source-file \"missing.foo\" does not exist, skipping"
        , "Specified extra-source-file \"res/*\" does not exist, skipping"
        ], [])

    it "doesn't warn when there are redundant patterns" $ do
      let file = "res/hello"
      touch file
      fst <$> expandGlobs ["res/*", "res/hello"] `shouldReturn` []
