module HpackSpec (spec) where

import           Helper

import           Prelude ()
import           Prelude.Compat hiding (readFile)
import qualified Prelude.Compat as Prelude

import           Control.DeepSeq

import           Hpack.CabalFile (makeVersion)
import           Hpack hiding (hpack)

readFile :: FilePath -> IO String
readFile name = Prelude.readFile name >>= (return $!!)

spec :: Spec
spec = do
  describe "hpackWithVersion" $ do
    context "when only the hpack version in the cabal file header changed" $ do
      it "does not write a new cabal file" $ do
        inTempDirectory $ do
          writeFile "package.yaml" "name: foo"
          hpackWithVersion (makeVersion [0,8,0]) Nothing False
          old <- readFile "foo.cabal"
          hpackWithVersion (makeVersion [0,10,0]) Nothing False
          readFile "foo.cabal" `shouldReturn` old

    context "when exsting cabal file was generated with a newer version of hpack" $ do
      it "does not re-generate" $ do
        inTempDirectory $ do
          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "version: 0.1.0"
            ]
          hpackWithVersion (makeVersion [0,10,0]) Nothing False
          old <- readFile "foo.cabal"

          writeFile "package.yaml" $ unlines [
              "name: foo"
            , "version: 0.2.0"
            ]

          hpackWithVersion (makeVersion [0,8,0]) Nothing False
          readFile "foo.cabal" `shouldReturn` old

  describe "splitDirectory" $ do
    context "when given Nothing" $ do
      it "defaults file name to package.yaml" $ do
        splitDirectory Nothing `shouldReturn` (Nothing, "package.yaml")

    context "when given a directory" $ do
      it "defaults file name to package.yaml" $ do
        withTempDirectory $ \dir -> do
          splitDirectory (Just dir) `shouldReturn` (Just dir, "package.yaml")

    context "when given a file name" $ do
      it "defaults directory to Nothing" $ do
        inTempDirectory $ do
          touch "foo.yaml"
          splitDirectory (Just "foo.yaml") `shouldReturn` (Nothing, "foo.yaml")

    context "when given a path to a file" $ do
      it "splits directory from file name" $ do
        withTempDirectory $ \dir -> do
          let file = dir </> "foo.yaml"
          touch file
          splitDirectory (Just file) `shouldReturn` (Just dir, "foo.yaml")

    context "when path does not exist" $ do
      it "defaults directory to Nothing" $ do
        inTempDirectory $ do
          splitDirectory (Just "test/foo.yaml") `shouldReturn` (Just "test", "foo.yaml")

    context "when file does not exist" $ do
      it "defaults directory to Nothing" $ do
        inTempDirectory $ do
          splitDirectory (Just "test") `shouldReturn` (Nothing, "test")

    context "when directory does not exist" $ do
      it "defaults directory to Nothing" $ do
        inTempDirectory $ do
          splitDirectory (Just "test/") `shouldReturn` (Just "test", "package.yaml")
