module HpackSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq
import           Data.Version (Version(..))

import           Test.Hspec
import           Test.Mockery.Directory

import           Hpack

makeVersion :: [Int] -> Version
makeVersion v = Version v []

spec :: Spec
spec = do
  describe "parseVerbosity" $ do
    it "returns True by default" $ do
      parseVerbosity ["foo"] `shouldBe` (True, ["foo"])

    context "with --silent" $ do
      it "returns False" $ do
        parseVerbosity ["--silent"] `shouldBe` (False, [])

  describe "hpackWithVersion" $ do
    context "when only the hpack version in the cabal file header changed" $ do
      it "does not write a new cabal file" $ do
        inTempDirectory $ do
          writeFile "package.yaml" "name: foo"
          hpackWithVersion (makeVersion [0,8,0]) "." False
          old <- readFile "foo.cabal" >>= (return $!!)
          hpackWithVersion (makeVersion [0,10,0]) "." False
          readFile "foo.cabal" `shouldReturn` old
