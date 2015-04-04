{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (main, spec) where

import           Test.Hspec
import           Helper
import           Data.String.Interpolate

import           Config

main :: IO ()
main = hspec spec

package :: String -> Package
package name = Package name "0.0.0" Nothing Nothing Nothing [] []

executable :: String -> String -> Executable
executable name main_ = Executable name main_ [] [] []

library :: Library
library = Library [] [] [] []

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "readConfig" $ do
    it "reads package config" $ do
      writeFile "package.yaml" [i|
        name: foo
        |]
      readConfig "package.yaml" `shouldReturn` Just (package "foo")

    it "accepts version" $ do
      writeFile "package.yaml" [i|
        name: foo
        version: 0.1.0
        |]
      readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageVersion = "0.1.0"}

    it "accepts license" $ do
      writeFile "package.yaml" [i|
        name: foo
        license: MIT
        |]
      readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageLicense = Just "MIT"}

    it "infers license file" $ do
      writeFile "package.yaml" [i|
        name: foo
        |]
      touch "LICENSE"
      readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageLicenseFile = Just "LICENSE"}

    context "when reading library section" $ do
      it "allows to specify exposed modules" $ do
        writeFile "package.yaml" [i|
          name: foo
          library:
            exposed-modules: Foo
          |]
        touch "src/Foo.hs"
        touch "src/Bar.hs"
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageLibrary = Just library {libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}}

    context "when reading executable section" $ do
      it "reads executable section" $ do
        writeFile "package.yaml" [i|
          name: foo
          executables:
            foo:
              main: driver/Main.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageExecutables = [executable "foo" "driver/Main.hs"]}

      it "accepts GHC options" $ do
        writeFile "package.yaml" [i|
          name: foo
          executables:
            foo:
              main: driver/Main.hs
              ghc-options: -Wall
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageExecutables = [(executable "foo" "driver/Main.hs") {executableGhcOptions = ["-Wall"]}]}

      it "accepts global GHC options" $ do
        writeFile "package.yaml" [i|
          name: foo
          ghc-options: -Wall
          executables:
            foo:
              main: driver/Main.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageExecutables = [(executable "foo" "driver/Main.hs") {executableGhcOptions = ["-Wall"]}]}

      it "accepts source-dirs" $ do
        writeFile "package.yaml" [i|
          name: foo
          executables:
            foo:
              main: Main.hs
              source-dirs:
                - src
                - driver
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageExecutables = [(executable "foo" "Main.hs") {executableSourceDirs = ["src", "driver"]}]}

    context "when reading test section" $ do
      it "reads test section" $ do
        writeFile "package.yaml" [i|
          name: foo
          tests:
            spec:
              main: test/Spec.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [executable "spec" "test/Spec.hs"]}

      it "accepts single dependency" $ do
        writeFile "package.yaml" [i|
          name: foo
          tests:
            spec:
              main: test/Spec.hs
              dependencies: hspec
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = ["hspec"]}]}

      it "accepts list of dependencies" $ do
        writeFile "package.yaml" [i|
          name: foo
          tests:
            spec:
              main: test/Spec.hs
              dependencies:
                - hspec
                - QuickCheck
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = ["hspec", "QuickCheck"]}]}

      context "when both top-level and section specific dependencies are specified" $ do
        it "combines dependencies" $ do
          writeFile "package.yaml" [i|
            name: foo
            dependencies:
              - base

            tests:
              spec:
                main: test/Spec.hs
                dependencies: hspec
            |]
          readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = ["base", "hspec"]}]}
