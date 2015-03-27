{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module ConfigSpec (main, spec) where

import           Test.Hspec
import           Helper
import           Data.String.Interpolate

import           Config hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readConfig" $ do
    it "reads package config" $ do
      withFile [i|
dependencies:
  - base

tests:
  spec: 
    main: test/Spec.hs
      |] $ \file -> readConfig file `shouldReturn` Just (Config ["base"] [("spec", Test "test/Spec.hs")])
