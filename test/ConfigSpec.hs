{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (main, spec) where

import           Test.Hspec
import           Helper
import           Data.String.Interpolate

import           Config

main :: IO ()
main = hspec spec

package :: Package
package = Package "foo" "0.0.0" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []

executable :: String -> String -> Executable
executable name main_ = Executable name main_ [] [] []

library :: Library
library = Library [] [] [] []

spec :: Spec
spec = around_ (inTempDirectory "foo") $ do
  describe "readConfig" $ do
    it "accepts name" $ do
      writeFile "package.yaml" [i|
        name: bar
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageName = "bar"}

    it "accepts version" $ do
      writeFile "package.yaml" [i|
        version: 0.1.0
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageVersion = "0.1.0"}

    it "accepts synopsis" $ do
      writeFile "package.yaml" [i|
        synopsis: some synopsis
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageSynopsis = Just "some synopsis"}

    it "accepts description" $ do
      writeFile "package.yaml" [i|
        description: some description
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageDescription = Just "some description"}

    it "accepts category" $ do
      writeFile "package.yaml" [i|
        category: Data
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageCategory = Just "Data"}

    it "accepts author" $ do
      writeFile "package.yaml" [i|
        author: John Doe
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageAuthor = Just "John Doe"}

    it "accepts maintainer" $ do
      writeFile "package.yaml" [i|
        maintainer: John Doe <john.doe@example.com>
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageMaintainer = Just "John Doe <john.doe@example.com>"}

    it "accepts copyright" $ do
      writeFile "package.yaml" [i|
        copyright: (c) 2015 John Doe
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageCopyright = Just "(c) 2015 John Doe"}

    it "accepts license" $ do
      writeFile "package.yaml" [i|
        license: MIT
        |]
      readConfig "package.yaml" `shouldReturn` Just package {packageLicense = Just "MIT"}

    it "infers license file" $ do
      writeFile "package.yaml" [i|
        name: foo
        |]
      touch "LICENSE"
      readConfig "package.yaml" `shouldReturn` Just package {packageLicenseFile = Just "LICENSE"}

    context "when reading library section" $ do
      it "allows to specify exposed-modules" $ do
        writeFile "package.yaml" [i|
          library:
            exposed-modules: Foo
          |]
        touch "src/Foo.hs"
        touch "src/Bar.hs"
        readConfig "package.yaml" `shouldReturn` Just package {packageLibrary = Just library {libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}}

      it "allows to specify other-modules" $ do
        writeFile "package.yaml" [i|
          library:
            other-modules: Bar
          |]
        touch "src/Foo.hs"
        touch "src/Bar.hs"
        readConfig "package.yaml" `shouldReturn` Just package {packageLibrary = Just library {libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}}

      it "allows to specify both exposed-modules and other-modules" $ do
        writeFile "package.yaml" [i|
          library:
            exposed-modules: Foo
            other-modules: Bar
          |]
        touch "src/Baz.hs"
        readConfig "package.yaml" `shouldReturn` Just package {packageLibrary = Just library {libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}}

      context "when neither exposed-module nor other-module are specified" $ do
        it "exposes all modules" $ do
          writeFile "package.yaml" [i|
            library: {}
            |]
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          readConfig "package.yaml" `shouldReturn` Just package {packageLibrary = Just library {libraryExposedModules = ["Bar", "Foo"]}}

    context "when reading executable section" $ do
      it "reads executable section" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              main: driver/Main.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageExecutables = [executable "foo" "driver/Main.hs"]}

      it "accepts GHC options" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              main: driver/Main.hs
              ghc-options: -Wall
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageExecutables = [(executable "foo" "driver/Main.hs") {executableGhcOptions = ["-Wall"]}]}

      it "accepts global GHC options" $ do
        writeFile "package.yaml" [i|
          ghc-options: -Wall
          executables:
            foo:
              main: driver/Main.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageExecutables = [(executable "foo" "driver/Main.hs") {executableGhcOptions = ["-Wall"]}]}

      it "accepts source-dirs" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              main: Main.hs
              source-dirs:
                - src
                - driver
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageExecutables = [(executable "foo" "Main.hs") {executableSourceDirs = ["src", "driver"]}]}

    context "when reading test section" $ do
      it "reads test section" $ do
        writeFile "package.yaml" [i|
          tests:
            spec:
              main: test/Spec.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageTests = [executable "spec" "test/Spec.hs"]}

      it "accepts single dependency" $ do
        writeFile "package.yaml" [i|
          tests:
            spec:
              main: test/Spec.hs
              dependencies: hspec
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = ["hspec"]}]}

      it "accepts list of dependencies" $ do
        writeFile "package.yaml" [i|
          tests:
            spec:
              main: test/Spec.hs
              dependencies:
                - hspec
                - QuickCheck
          |]
        readConfig "package.yaml" `shouldReturn` Just package {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = ["hspec", "QuickCheck"]}]}

      context "when both top-level and section specific dependencies are specified" $ do
        it "combines dependencies" $ do
          writeFile "package.yaml" [i|
            dependencies:
              - base

            tests:
              spec:
                main: test/Spec.hs
                dependencies: hspec
            |]
          readConfig "package.yaml" `shouldReturn` Just package {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = ["base", "hspec"]}]}
