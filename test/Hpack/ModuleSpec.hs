{-# LANGUAGE OverloadedStrings #-}
module Hpack.ModuleSpec (spec) where

import           Helper

import           Hpack.Module

spec :: Spec
spec = do
  describe "getModules" $ around withTempDirectory $ do
    it "returns Haskell modules in specified source directory" $ \dir -> do
      touch (dir </> "src/Foo.hs")
      touch (dir </> "src/Bar/Baz.hs")
      touch (dir </> "src/Setup.hs")
      getModules dir "src" >>= (`shouldMatchList` ["Foo", "Bar.Baz", "Setup"])

    context "when source directory is '.'" $ do
      it "ignores Setup" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Setup.hs")
        getModules dir  "." `shouldReturn` ["Foo"]

    context "when source directory is './.'" $ do
      it "ignores Setup" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Setup.hs")
        getModules dir  "./." `shouldReturn` ["Foo"]

  describe "toModule" $ do
    it "maps .hs paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.hs"]  `shouldBe` Just "Foo.Bar.Baz"

    it "maps .lhs paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.lhs"] `shouldBe` Just "Foo.Bar.Baz"

    it "maps .hsc paths to module names" $ do
      toModule ["Foo", "Bar", "Baz.hsc"] `shouldBe` Just "Foo.Bar.Baz"

    it "rejects invalid module names" $ do
      toModule ["resources", "hello.hs"] `shouldBe` Nothing

  describe "getModuleFilesRecursive" $ do
    it "gets all files from given directory" $ do
      inTempDirectory $ do
        touch "foo/bar"
        touch "foo/baz"
        actual <- getModuleFilesRecursive "foo"
        actual `shouldMatchList` [
            ["bar"]
          , ["baz"]
          ]

    it "descends into subdirectories" $ do
      inTempDirectory $ do
        touch "foo/Bar/baz"
        getModuleFilesRecursive "foo" `shouldReturn` [["Bar", "baz"]]

    context "when a subdirectory is not a valid module name" $ do
      it "does not descend" $ do
        inTempDirectory $ do
          touch "foo/bar/baz"
          getModuleFilesRecursive "foo" `shouldReturn` empty
