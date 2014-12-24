module UtilSpec (main, spec) where

import           Test.Hspec

import           Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toModule" $ do
    it "maps a path to a module name" $ do
      toModule "Foo/Bar/Baz.hs" `shouldBe` Just "Foo.Bar.Baz"
