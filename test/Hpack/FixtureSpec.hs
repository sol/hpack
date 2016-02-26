{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Hpack.FixtureSpec (
  spec
) where

import           Helper

import           Data.FileEmbed (embedFile, embedStringFile)

import           Hpack.Config (readPackageConfigBS)
import           Hpack.Render (defaultRenderSettings)
import           Hpack.Run    (renderPackage)

spec :: Spec
spec = do
  describe "fixtures" $ do
    context "flags" $ do
      it "parsed correctly" $ do
        let cabal = $(embedStringFile "test/fixtures/flags.cabal")
        Right (_, c) <- readPackageConfigBS $(embedFile "test/fixtures/flags.yaml")
        let r = renderPackage defaultRenderSettings 0 [] c
        r `shouldBe` cabal
