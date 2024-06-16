{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Hpack.DefaultsSpec (spec) where

import           Helper
import           System.Directory

import           Hpack.Error
import           Hpack.Syntax.Defaults
import           Hpack.Defaults

spec :: Spec
spec = do
  describe "ensure" $ do
    it "fails when local file does not exist" $ do
      cwd <- getCurrentDirectory
      let expected = Left (DefaultsFileNotFound $ cwd </> "foo")
      ensure undefined cwd (DefaultsLocal $ Local "foo") `shouldReturn` expected

  describe "ensureFile" $ do
    let
      file = "foo"
      url = "https://raw.githubusercontent.com/sol/hpack/master/Setup.lhs"

    it "downloads file if missing" $ do
      pending
      expected <- readFile "Setup.lhs"
      inTempDirectory $ do
        Found <- ensureFile file url
        readFile file `shouldReturn` expected

    context "with existing file" $ do
      it "does nothing" $ do
        let expected = "contents of existing file"
        inTempDirectory $ do
          writeFile file expected
          Found <- ensureFile file url
          readFile file `shouldReturn` expected

    context "with 404" $ do
      let
        url = "https://raw.githubusercontent.com/sol/hpack/master/Setup.foo"

      it "does not create any files" $ do
        pending
        inTempDirectory $ do
          NotFound <- ensureFile file url
          doesFileExist file `shouldReturn` False
