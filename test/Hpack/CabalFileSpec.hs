module Hpack.CabalFileSpec (spec) where

import           Helper
import           Test.QuickCheck
import           Data.Version (showVersion)
import           Control.Monad.Compat

import           Hpack.CabalFile

spec :: Spec
spec = do
  describe "extractVersion" $ do
    it "extracts Hpack version from a cabal file" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version 0.10.0."]
      extractVersion cabalFile `shouldBe` Just (makeVersion [0, 10, 0])

    it "is total" $ do
      let cabalFile = ["-- This file has been generated from package.yaml by hpack version "]
      extractVersion cabalFile `shouldBe` Nothing

  describe "parseVersion" $ do
    it "is inverse to showVersion" $ do
      let positive = getPositive <$> arbitrary
      forAll (replicateM 3 positive) $ \xs -> do
        let v = makeVersion xs
        parseVersion (showVersion v) `shouldBe` Just v
