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
      getModules dir [] "src" >>= (`shouldMatchList` ["Foo", "Bar.Baz", "Setup"])

    context "when source directory is '.'" $ do
      it "ignores Setup" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Setup.hs")
        getModules dir  [] "." `shouldReturn` ["Foo"]

    context "when source directory is './.'" $ do
      it "ignores Setup" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Setup.hs")
        getModules dir  [] "./." `shouldReturn` ["Foo"]

    context "with a list of paths to exclude" $ do
      it "does not return files in that list" $ \dir -> do
        touch (dir </> "src/Foo.hs")
        touch (dir </> "src/Bar.hs")
        let exclude = ["src/Foo.hs"]
        getModules dir exclude "src" >>= (`shouldMatchList` ["Bar"])

      it "works for '.'" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Bar.hs")
        let exclude = ["Foo.hs"]
        getModules dir exclude "." >>= (`shouldMatchList` ["Bar"])

  describe "toModule" $ do
    it "maps a Path to a Module" $ do
      toModule "Foo/Bar/Baz.hs" `shouldBe` "Foo.Bar.Baz"

  describe "getModuleFilesRecursive" $ do
    it "gets all Haskell source files from given directory" $ do
      inTempDirectory $ do
        touch "foo/Bar.hs"
        touch "foo/Baz.chs"
        actual <- getModuleFilesRecursive "foo"
        actual `shouldMatchList` [
            "Bar.hs"
          , "Baz.chs"
          ]

    it "ignores other files" $ do
      inTempDirectory $ do
        touch "foo/Bar.js"
        getModuleFilesRecursive "foo" `shouldReturn` []

    it "descends into subdirectories" $ do
      inTempDirectory $ do
        touch "foo/Bar/Baz.hs"
        getModuleFilesRecursive "foo" `shouldReturn` ["Bar/Baz.hs"]

    context "when a subdirectory is not a valid module name" $ do
      it "does not descend" $ do
        inTempDirectory $ do
          touch "foo/bar/Baz.hs"
          getModuleFilesRecursive "foo" `shouldReturn` empty
