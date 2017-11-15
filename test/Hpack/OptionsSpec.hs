module Hpack.OptionsSpec (spec) where

import           Helper

import           Prelude ()
import           Prelude.Compat

import           Hpack.Options

spec :: Spec
spec = do
  describe "parseOptions" $ do
    context "with --help" $ do
      it "returns Help" $ do
        parseOptions ["--help"] `shouldBe` Help

    context "with --version" $ do
      it "returns PrintVersion" $ do
        parseOptions ["--version"] `shouldBe` PrintVersion

    context "by default" $ do
      it "returns Run" $ do
        parseOptions [] `shouldBe` Run (Options True False False Nothing)

      it "includes target" $ do
        parseOptions ["foo"] `shouldBe` Run (Options True False False (Just "foo"))

      context "with superfluous arguments" $ do
        it "returns ParseError" $ do
          parseOptions ["foo", "bar"] `shouldBe` ParseError

      context "with --silent" $ do
        it "sets optionsVerbose to False" $ do
          parseOptions ["--silent"] `shouldBe` Run (Options False False False Nothing)

      context "with --force" $ do
        it "sets optionsForce to True" $ do
          parseOptions ["--force"] `shouldBe` Run (Options True True False Nothing)

      context "with -f" $ do
        it "sets optionsForce to True" $ do
          parseOptions ["-f"] `shouldBe` Run (Options True True False Nothing)

      context "with -" $ do
        it "sets optionsToStdout to True" $ do
          parseOptions ["-"] `shouldBe` Run (Options True False True Nothing)

        it "rejects - for target" $ do
          parseOptions ["-", "-"] `shouldBe` ParseError
