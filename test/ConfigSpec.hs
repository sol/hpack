{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (main, spec) where

import           Test.Hspec
import           Helper
import           Data.String.Interpolate

import           Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "readConfig" $ do
    it "reads package config" $ do
      writeFile "package.yaml" [i|
name: cabalize
dependencies:
  - base

tests:
  spec: 
    main: test/Spec.hs
      |]
      readConfig "package.yaml" `shouldReturn` Just (ConfigFile "cabalize" ["base"] [("spec", TestSection "test/Spec.hs" Nothing)])
