{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (main, spec) where

import           Test.Hspec
import           Helper
import           Data.String.Interpolate

import           Config

main :: IO ()
main = hspec spec

package :: String -> Package
package name = Package name [0,0,0] (Library [] []) []

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "readConfig" $ do
    it "reads package config" $ do
      writeFile "package.yaml" [i|
        name: foo
        dependencies:
          - base

        tests:
          spec:
            main: test/Spec.hs
        |]
      readConfig "package.yaml" `shouldReturn` Just (package "foo") {packageTests = [Test "spec" "test/Spec.hs" ["base"]], packageLibrary = Library [] ["base"]}
