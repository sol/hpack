{-# LANGUAGE QuasiQuotes #-}
module Hpack.Syntax.GitSpec (spec) where

import           Helper
import           Data.String.Interpolate

import           Hpack.Syntax.Git

spec :: Spec
spec = do
  describe "isValidRef" $ do
    it "accepts slashes" $ do
      isValidRef "foo/bar" `shouldBe` True

    it "rejects the empty string" $ do
      isValidRef "" `shouldBe` False

    it "accepts .lock as a substring" $ do
      isValidRef "foo.locking" `shouldBe` True

    it "rejects .lock at the end of a component" $ do
      isValidRef "foo/bar.lock/baz" `shouldBe` False

    it "rejects . at the beginning of a component" $ do
      isValidRef "foo/.bar/baz" `shouldBe` False

    it "rejects two consecutive dots .." $ do
      isValidRef "foo..bar" `shouldBe` False

    it "rejects ASCII control characters" $ do
      isValidRef "foo\10bar" `shouldBe` False

    it "rejects space" $ do
      isValidRef "foo bar" `shouldBe` False

    forM_ ["~", "^", ":", "?", "*", "[", "\\"] $ \ xs -> do
      it [i|rejects #{xs}|] $ do
        isValidRef [i|foo#{xs}bar|] `shouldBe` False

    it "rejects multiple consecutive slashes" $ do
      isValidRef "foo//bar" `shouldBe` False

    it "rejects slash at beginning" $ do
      isValidRef "/foo" `shouldBe` False

    it "rejects slash at end" $ do
      isValidRef "foo/" `shouldBe` False

    it "rejects . at end" $ do
      isValidRef "foo." `shouldBe` False

    it "rejects @{" $ do
      isValidRef "foo@{bar" `shouldBe` False

    it "rejects the single character @" $ do
      isValidRef "@" `shouldBe` False
