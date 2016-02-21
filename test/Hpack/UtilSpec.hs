{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Hpack.UtilSpec (main, spec) where

import           Data.Aeson
import           Data.Aeson.QQ
import           Data.Aeson.Types
import           Helper
import           System.Directory

import           Hpack.Config
import           Hpack.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sort" $ do
    it "sorts lexicographically" $ do
      sort ["foo", "Foo"] `shouldBe` ["Foo", "foo" :: String]

  describe "parseMain" $ do
    it "accepts source file" $ do
      parseMain "Main.hs" `shouldBe` ("Main.hs", [])

    it "accepts literate source file" $ do
      parseMain "Main.lhs" `shouldBe` ("Main.lhs", [])

    it "accepts module" $ do
      parseMain "Foo" `shouldBe` ("Foo.hs", ["-main-is Foo"])

    it "accepts hierarchical module" $ do
      parseMain "Foo.Bar.Baz" `shouldBe` ("Foo/Bar/Baz.hs", ["-main-is Foo.Bar.Baz"])

    it "accepts qualified identifier" $ do
      parseMain "Foo.bar" `shouldBe` ("Foo.hs", ["-main-is Foo.bar"])

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
        actual <- getFilesRecursive "foo"
        actual `shouldMatchList` [
            ["bar"]
          , ["baz"]
          , ["foobar", "baz"]
          ]

  describe "List" $ do
    let invalid = [aesonQQ|{
          name: "hpack",
          gi: "sol/hpack",
          ref: "master"
        }|]
        parseError :: String -> Either String (List Dependency)
        parseError prefix = Left (prefix ++ ": neither key \"git\" nor key \"github\" present")
    context "when parsing single values" $ do
      it "returns the value in a singleton list" $ do
        fromJSON (toJSON $ Number 23) `shouldBe` Success (List [23 :: Int])

      it "returns error messages from element parsing" $ do
        parseEither parseJSON invalid `shouldBe` parseError "Error in $"

    context "when parsing a list of values" $ do
      it "returns the list" $ do
        fromJSON (toJSON [Number 23, Number 42]) `shouldBe` Success (List [23, 42 :: Int])

      it "propagates parse error messages of invalid elements" $ do
        parseEither parseJSON (toJSON [String "foo", invalid]) `shouldBe` parseError "Error in $[1]"

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

  describe "expandGlobs" $ around withTempDirectory $ do
    it "accepts simple files" $ \dir -> do
        touch (dir </> "foo.js")
        expandGlobs dir ["foo.js"] `shouldReturn` ([], ["foo.js"])

    it "removes duplicates" $ \dir -> do
      touch (dir </> "foo.js")
      expandGlobs dir ["foo.js", "*.js"] `shouldReturn` ([], ["foo.js"])

    it "rejects directories" $ \dir -> do
      touch (dir </> "foo")
      createDirectory (dir </> "bar")
      expandGlobs dir ["*"] `shouldReturn` ([], ["foo"])

    it "rejects character ranges" $ \dir -> do
      touch (dir </> "foo1")
      touch (dir </> "foo2")
      touch (dir </> "foo[1,2]")
      expandGlobs dir ["foo[1,2]"] `shouldReturn` ([], ["foo[1,2]"])

    context "when expanding *" $ do
      it "expands by extension" $ \dir -> do
        let files = [
                "files/foo.js"
              , "files/bar.js"
              , "files/baz.js"]
        mapM_ (touch . (dir </>)) files
        touch (dir </> "files/foo.hs")
        expandGlobs dir ["files/*.js"] `shouldReturn` ([], sort files)

      it "rejects dot-files" $ \dir -> do
        touch (dir </> "foo/bar")
        touch (dir </> "foo/.baz")
        expandGlobs dir ["foo/*"] `shouldReturn` ([], ["foo/bar"])

      it "accepts dot-files when explicitly asked to" $ \dir -> do
        touch (dir </> "foo/bar")
        touch (dir </> "foo/.baz")
        expandGlobs dir ["foo/.*"] `shouldReturn` ([], ["foo/.baz"])

      it "matches at most one directory component" $ \dir -> do
        touch (dir </> "foo/bar/baz.js")
        touch (dir </> "foo/bar.js")
        expandGlobs dir ["*/*.js"] `shouldReturn` ([], ["foo/bar.js"])

    context "when expanding **" $ do
      it "matches arbitrary many directory components" $ \dir -> do
        let file = "foo/bar/baz.js"
        touch (dir </> file)
        expandGlobs dir ["**/*.js"] `shouldReturn` ([], [file])

    context "when a pattern does not match anything" $ do
      it "warns" $ \dir -> do
        expandGlobs dir ["foo"] `shouldReturn`
          (["Specified pattern \"foo\" for extra-source-files does not match any files"], [])

    context "when a pattern only matches a directory" $ do
      it "warns" $ \dir -> do
        createDirectory (dir </> "foo")
        expandGlobs dir ["foo"] `shouldReturn`
          (["Specified pattern \"foo\" for extra-source-files does not match any files"], [])
