{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (main, spec) where

import           Test.Hspec
import           Helper
import           Data.String.Interpolate

import           Config

main :: IO ()
main = hspec spec

package :: String -> Package
package name = Package name [0,0,0] (Library [] []) [] []

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "readConfig" $ do
    it "reads package config" $ do
      writeFile "package.yaml" [i|
        name: foo
        |]
      readConfig "package.yaml" `shouldReturn` Just (package "foo")

    context "when reading executable section" $ do
      it "reads executable section" $ do
        writeFile "package.yaml" [i|
          name: foo
          executables:
            foo:
              main: test/Spec.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageExecutables = [Executable "foo" "test/Spec.hs" []]}

    context "when reading test section" $ do
      it "reads test section" $ do
        writeFile "package.yaml" [i|
          name: foo
          tests:
            spec:
              main: test/Spec.hs
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [Executable "spec" "test/Spec.hs" []]}

      it "accepts single dependency" $ do
        writeFile "package.yaml" [i|
          name: foo
          tests:
            spec:
              main: test/Spec.hs
              dependencies: hspec
          |]
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [Executable "spec" "test/Spec.hs" ["hspec"]]}

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
        readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [Executable "spec" "test/Spec.hs" ["hspec", "QuickCheck"]]}

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
          readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [Executable "spec" "test/Spec.hs" ["base", "hspec"]], packageLibrary = Library [] ["base"]}
