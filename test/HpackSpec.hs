module HpackSpec (spec) where

import           Helper

import           Prelude hiding (readFile)
import qualified Prelude as Prelude

import           Control.DeepSeq
import           System.Directory (createDirectory)
import           Test.Hspec (shouldContain)

import           Hpack.Config
import           Hpack.CabalFile
import           Hpack hiding (hpack)

readFile :: FilePath -> IO String
readFile name = Prelude.readFile name >>= (return $!!)

spec :: Spec
spec = do
  describe "hpackResult" $ do
    context "with existing cabal file" $ around_ inTempDirectory $ before_ (writeFile packageConfig "name: foo") $ do
      let
        file = "foo.cabal"

        hpackWithVersion v = hpackResultWithVersion v defaultOptions
        hpack = hpackResult defaultOptions
        hpackForce = hpackResult defaultOptions {optionsForce = Force}

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

    context "with an absolute path to local defaults" $ do
      it "resolves the path correctly" $ do
        withTempDirectory $ flip withCurrentDirectory $ do
          withTempDirectory $ \ defaultsDir -> do
            writeFile (defaultsDir </> "defaults.yaml") "ghc-options: A"
            writeFile "package.yaml" $ unlines
              [ "name: foo"
              , "defaults:"
              , "  local: " ++ (defaultsDir </> "defaults.yaml")
              , "executable:"
              , "  main: Main.hs"
              ]
            _ <- hpackResult $ setTarget "package.yaml" defaultOptions
            res <- readFile "foo.cabal"
            res `shouldContain` "ghc-options: A"

    context "with a relative path to local defaults" $ do
      it "resolves relative to package.yaml, not CWD" $ do
        withTempDirectory $ flip withCurrentDirectory $ do
          createDirectory "foo"
          writeFile "defaults.yaml" "ghc-options: A"
          writeFile "foo/defaults.yaml" "ghc-options: B"
          writeFile "foo/package.yaml" $ unlines
            [ "name: foo"
            , "defaults:"
            , "  local: defaults.yaml"
            , "executable:"
            , "  main: Main.hs"
            ]
          _ <- hpackResult $ setTarget "foo/package.yaml" defaultOptions
          fromOutside <- readFile "foo/foo.cabal"
          _ <- withCurrentDirectory "foo" $ hpackResult defaultOptions
          readFile "foo/foo.cabal" `shouldReturn` fromOutside

      it "does not have false positives in cycle detection due to same-named default files in different directories" $ do
        withTempDirectory $ flip withCurrentDirectory $ do
          createDirectory "x"
          writeFile "defaults-a.yaml" $ unlines
            [ "defaults:"
            , "  local: defaults-b.yaml"
            ]
          writeFile "defaults-b.yaml" $ unlines
            [ "defaults:"
            , "  local: x/defaults-b.yaml"
            ]
          writeFile "x/defaults-b.yaml" $ unlines
            [ "defaults:"
            , "  local: defaults-a.yaml"
            ]
          writeFile "x/defaults-a.yaml" "ghc-options: X"
          writeFile "package.yaml" $ unlines
            [ "name: foo"
            , "defaults:"
            , "  local: defaults-a.yaml"
            , "executable:"
            , "  main: Main.hs"
            ]
          _ <- hpackResult defaultOptions
          res <- readFile "foo.cabal"
          res `shouldContain` "ghc-options: X"
