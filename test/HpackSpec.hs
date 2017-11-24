module HpackSpec (spec) where

import           Helper

import           Prelude hiding (readFile)
import qualified Prelude as Prelude

import           Control.DeepSeq

import           Hpack.Config (packageConfig)
import           Hpack.CabalFile
import           Hpack hiding (hpack)

readFile :: FilePath -> IO String
readFile name = Prelude.readFile name >>= (return $!!)

spec :: Spec
spec = do
  describe "hpackWithVersionResult" $ do
    context "with existing cabal file" $ around_ inTempDirectory $ before_ (writeFile packageConfig "name: foo") $ do
      let
        file = "foo.cabal"

        hpackWithVersion v = hpackWithVersionResult v Nothing NoForce
        hpack = hpackWithVersionResult version Nothing NoForce
        hpackForce = hpackWithVersionResult version Nothing Force

        generated = Result [] file Generated
        modifiedManually = Result [] file ExistingCabalFileWasModifiedManually
        outputUnchanged = Result [] file OutputUnchanged
        alreadyGeneratedByNewerHpack = Result [] file AlreadyGeneratedByNewerHpack

      context "when cabal file was created manually" $ do
        it "does not overwrite existing cabal file" $ do
          let existing = "some existing cabal file"
          writeFile file existing
          hpack `shouldReturn` modifiedManually
          readFile file `shouldReturn` existing

        context "with --force" $ do
          it "overwrites existing cabal file" $ do
            _ <- hpack
            expected <- readFile file
            writeFile file "some existing cabal file"
            hpackForce `shouldReturn` generated
            readFile file `shouldReturn` expected

      context "when cabal file was created with hpack < 0.20.0" $ do
        it "overwrites existing cabal file" $ do
          _ <- hpack
          expected <- readFile file
          writeFile file "-- This file has been generated from package.yaml by hpack version 0.19.3."
          hpack `shouldReturn` generated
          readFile file `shouldReturn` expected

      context "when cabal file was created with hpack >= 0.20.0" $ do
        context "when hash is missing" $ do
          it "does not overwrite existing cabal file" $ do
            let existing = "-- This file has been generated from package.yaml by hpack version 0.20.0."
            writeFile file existing
            hpack `shouldReturn` modifiedManually
            readFile file `shouldReturn` existing

        context "when hash is present" $ do
          context "when exsting cabal file was generated with a newer version of hpack" $ do
            it "does not overwrite existing cabal file" $ do
              writeFile packageConfig $ unlines [
                  "name: foo"
                , "version: 0.1.0"
                ]
              _ <- hpackWithVersion (makeVersion [0,22,0])
              old <- readFile file

              writeFile packageConfig $ unlines [
                  "name: foo"
                , "version: 0.2.0"
                ]

              hpackWithVersion (makeVersion [0,20,0]) `shouldReturn` alreadyGeneratedByNewerHpack
              readFile file `shouldReturn` old

          context "when cabal file was modified manually" $ do
            it "does not overwrite existing cabal file" $ do
              _ <- hpack
              old <- readFile file
              let modified = old ++ "foo\n"
              writeFile file modified
              _ <- hpack
              readFile file `shouldReturn` modified

          context "when only the hpack version in the cabal file header changed" $ do
            it "does not overwrite existing cabal file" $ do
              _ <- hpackWithVersion (makeVersion [0,20,0])
              old <- readFile file
              hpack `shouldReturn` outputUnchanged
              readFile file `shouldReturn` old

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
