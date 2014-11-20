{-# LANGUAGE QuasiQuotes #-}
module CabalizeSpec (main, spec) where

import           Test.Hspec
import           Data.String.Interpolate
import           Data.String.Interpolate.Util

import           Cabalize

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "cabalize" $ do
    it "creates cabal file" $ do
      cabalize `shouldReturn` ("cabalize.cabal", unindent [i|
      -- This file has been generated from .package.yaml by Cabalize.
      name: cabalize
      version: 0.0.0
      build-type: Simple
      cabal-version: >= 1.10

      library
        hs-source-dirs: src
        exposed-modules:
            Cabalize
            Config
            Util
        build-depends:
            base == 4.*
          , base-compat
          , directory
          , filepath
          , interpolate
          , yaml
        default-language: Haskell2010
      |])
