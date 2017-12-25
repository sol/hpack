{-# LANGUAGE QuasiQuotes #-}
module Hpack.SyntaxSpec (spec) where

import           Helper
import           Data.String.Interpolate.IsString

import           Hpack.Syntax

spec :: Spec
spec = do
  describe "parseJSON" $ do
    context "when parsing Defaults" $ do
      it "accepts Defaults from GitHub" $ do
        [i|
        github: sol/hpack
        ref: 0.1.0
        path: defaults.yaml
        |] `shouldParseAs` Right (Defaults "sol/hpack" "0.1.0" "defaults.yaml")
