{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (
  main
, spec

, package
, executable
, library
) where

import           Helper

import           Data.String.Interpolate

import           Config

main :: IO ()
main = hspec spec

package :: Package
package = Package "foo" "0.0.0" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []

executable :: String -> String -> Executable
executable name main_ = Executable name main_ [] [] []

library :: Library
library = Library [] [] [] [] []

spec :: Spec
spec = around_ (inTempDirectory "foo") $ do
  describe "readConfig" $ do
    it "accepts name" $ do
      writeFile "package.yaml" [i|
        name: bar
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageName = "bar"}

    it "accepts version" $ do
      writeFile "package.yaml" [i|
        version: 0.1.0
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageVersion = "0.1.0"}

    it "accepts synopsis" $ do
      writeFile "package.yaml" [i|
        synopsis: some synopsis
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageSynopsis = Just "some synopsis"}

    it "accepts description" $ do
      writeFile "package.yaml" [i|
        description: some description
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageDescription = Just "some description"}

    it "accepts category" $ do
      writeFile "package.yaml" [i|
        category: Data
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageCategory = Just "Data"}

    it "accepts author" $ do
      writeFile "package.yaml" [i|
        author: John Doe
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageAuthor = Just "John Doe"}

    it "accepts maintainer" $ do
      writeFile "package.yaml" [i|
        maintainer: John Doe <john.doe@example.com>
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageMaintainer = Just "John Doe <john.doe@example.com>"}

    it "accepts copyright" $ do
      writeFile "package.yaml" [i|
        copyright: (c) 2015 John Doe
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageCopyright = Just "(c) 2015 John Doe"}

    it "accepts license" $ do
      writeFile "package.yaml" [i|
        license: MIT
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageLicense = Just "MIT"}

    it "infers license file" $ do
      writeFile "package.yaml" [i|
        name: foo
        |]
      touch "LICENSE"
      readConfig "package.yaml" `shouldReturn` Right package {packageLicenseFile = Just "LICENSE"}

    it "accepts github" $ do
      writeFile "package.yaml" [i|
        github: hspec/hspec
        |]
      readConfig "package.yaml" `shouldReturn` Right package {packageSourceRepository = Just "https://github.com/hspec/hspec"}

    context "when reading library section" $ do
      it "accepts source-dirs" $ do
        writeFile "package.yaml" [i|
          library:
            source-dirs:
              - foo
              - bar
          |]
        Right c <- readConfig "package.yaml"
        packageLibrary c `shouldBe` Just library {librarySourceDirs = ["foo", "bar"]}

      it "accepts global source-dirs" $ do
        writeFile "package.yaml" [i|
          source-dirs:
            - foo
            - bar
          library: {}
          |]
        Right c <- readConfig "package.yaml"
        packageLibrary c `shouldBe` Just library {librarySourceDirs = ["foo", "bar"]}

      it "allows to specify exposed-modules" $ do
        writeFile "package.yaml" [i|
          library:
            source-dirs: src
            exposed-modules: Foo
          |]
        touch "src/Foo.hs"
        touch "src/Bar.hs"
        Right c <- readConfig "package.yaml"
        packageLibrary c `shouldBe` Just library {librarySourceDirs = ["src"], libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}

      it "allows to specify other-modules" $ do
        writeFile "package.yaml" [i|
          library:
            source-dirs: src
            other-modules: Bar
          |]
        touch "src/Foo.hs"
        touch "src/Bar.hs"
        Right c <- readConfig "package.yaml"
        packageLibrary c `shouldBe` Just library {librarySourceDirs = ["src"], libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}

      it "allows to specify both exposed-modules and other-modules" $ do
        writeFile "package.yaml" [i|
          library:
            source-dirs: src
            exposed-modules: Foo
            other-modules: Bar
          |]
        touch "src/Baz.hs"
        Right c <- readConfig "package.yaml"
        packageLibrary c `shouldBe` Just library {librarySourceDirs = ["src"], libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}

      context "when neither exposed-module nor other-module are specified" $ do
        it "exposes all modules" $ do
          writeFile "package.yaml" [i|
            library:
              source-dirs: src
            |]
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          Right c <- readConfig "package.yaml"
          packageLibrary c `shouldBe` Just library {librarySourceDirs = ["src"], libraryExposedModules = ["Bar", "Foo"]}

    context "when reading executable section" $ do
      it "reads executable section" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              main: driver/Main.hs
          |]
        Right c <- readConfig "package.yaml"
        packageExecutables c `shouldBe` [executable "foo" "driver/Main.hs"]

      it "accepts source-dirs" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              main: Main.hs
              source-dirs:
                - foo
                - bar
          |]
        Right c <- readConfig "package.yaml"
        packageExecutables c `shouldBe` [(executable "foo" "Main.hs") {executableSourceDirs = ["foo", "bar"]}]

      it "accepts global source-dirs" $ do
        writeFile "package.yaml" [i|
          source-dirs:
            - foo
            - bar
          executables:
            foo:
              main: Main.hs
          |]
        Right c <- readConfig "package.yaml"
        packageExecutables c `shouldBe` [(executable "foo" "Main.hs") {executableSourceDirs = ["foo", "bar"]}]

      it "accepts GHC options" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              main: driver/Main.hs
              ghc-options: -Wall
          |]
        readConfig "package.yaml" `shouldReturn` Right package {packageExecutables = [(executable "foo" "driver/Main.hs") {executableGhcOptions = ["-Wall"]}]}

      it "accepts global GHC options" $ do
        writeFile "package.yaml" [i|
          ghc-options: -Wall
          executables:
            foo:
              main: driver/Main.hs
          |]
        readConfig "package.yaml" `shouldReturn` Right package {packageExecutables = [(executable "foo" "driver/Main.hs") {executableGhcOptions = ["-Wall"]}]}

    context "when reading test section" $ do
      it "reads test section" $ do
        writeFile "package.yaml" [i|
          tests:
            spec:
              main: test/Spec.hs
          |]
        readConfig "package.yaml" `shouldReturn` Right package {packageTests = [executable "spec" "test/Spec.hs"]}

      it "accepts single dependency" $ do
        writeFile "package.yaml" [i|
          tests:
            spec:
              main: test/Spec.hs
              dependencies: hspec
          |]
        readConfig "package.yaml" `shouldReturn` Right package {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = [["hspec"]]}]}

      it "accepts list of dependencies" $ do
        writeFile "package.yaml" [i|
          tests:
            spec:
              main: test/Spec.hs
              dependencies:
                - hspec
                - QuickCheck
          |]
        readConfig "package.yaml" `shouldReturn` Right package {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = [["hspec", "QuickCheck"]]}]}

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
          readConfig "package.yaml" `shouldReturn` Right package {packageTests = [(executable "spec" "test/Spec.hs") {executableDependencies = [["base"], ["hspec"]]}]}

    context "when package.yaml can not be parsed" $ do
      it "returns an error" $ do
        writeFile "package.yaml" [i|
          foo: bar
          foo baz
          |]
        readConfig "package.yaml" `shouldReturn` Left "package.yaml:3:10: could not find expected ':' while scanning a simple key"

    context "when package.yaml is invalid" $ do
      it "returns an error" $ do
        writeFile "package.yaml" [i|
          executables:
            foo:
              ain: driver/Main.hs
          |]
        readConfig "package.yaml" `shouldReturn` Left "package.yaml: The key \"main\" was not found"
