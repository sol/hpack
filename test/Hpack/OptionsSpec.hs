module Hpack.OptionsSpec (spec) where

import           Helper

import           Hpack.Options

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let configFile = "package.yaml"
    context "with --help" $ do
      it "returns Help" $ do
        parseOptions configFile ["--help"] `shouldReturn` Help

    context "with --version" $ do
      it "returns PrintVersion" $ do
        parseOptions configFile ["--version"] `shouldReturn` PrintVersion

    context "by default" $ do
      it "returns Run" $ do
        parseOptions configFile [] `shouldReturn` Run (Options Verbose NoForce False Nothing configFile)

      it "includes target" $ do
        parseOptions configFile ["foo.yaml"] `shouldReturn` Run (Options Verbose NoForce False Nothing "foo.yaml")

      context "with superfluous arguments" $ do
        it "returns ParseError" $ do
          parseOptions configFile ["foo", "bar"] `shouldReturn` ParseError

      context "with --silent" $ do
        it "sets optionsVerbose to NoVerbose" $ do
          parseOptions configFile ["--silent"] `shouldReturn` Run (Options NoVerbose NoForce False Nothing configFile)

      context "with --force" $ do
        it "sets optionsForce to Force" $ do
          parseOptions configFile ["--force"] `shouldReturn` Run (Options Verbose Force False Nothing configFile)

      context "with -f" $ do
        it "sets optionsForce to Force" $ do
          parseOptions configFile ["-f"] `shouldReturn` Run (Options Verbose Force False Nothing configFile)

      context "with -" $ do
        it "sets optionsToStdout to True" $ do
          parseOptions configFile ["-"] `shouldReturn` Run (Options Verbose NoForce True Nothing configFile)

        it "rejects - for target" $ do
          parseOptions configFile ["-", "-"] `shouldReturn` ParseError

  describe "splitDirectory" $ do
    let packageConfig = "package.yaml"
    context "when given Nothing" $ do
      it "defaults file name to package.yaml" $ do
        splitDirectory packageConfig Nothing `shouldReturn` (Nothing, "package.yaml")

    context "when given a directory" $ do
      it "defaults file name to package.yaml" $ do
        withTempDirectory $ \dir -> do
          splitDirectory packageConfig (Just dir) `shouldReturn` (Just dir, "package.yaml")

    context "when given a file name" $ do
      it "defaults directory to Nothing" $ do
        inTempDirectory $ do
          touch "foo.yaml"
          splitDirectory packageConfig (Just "foo.yaml") `shouldReturn` (Nothing, "foo.yaml")

    context "when given a path to a file" $ do
      it "splits directory from file name" $ do
        withTempDirectory $ \dir -> do
          let file = dir </> "foo.yaml"
          touch file
          splitDirectory packageConfig (Just file) `shouldReturn` (Just dir, "foo.yaml")

    context "when path does not exist" $ do
      it "splits path into directory and file name" $ do
        inTempDirectory $ do
          splitDirectory packageConfig (Just "test/foo.yaml") `shouldReturn` (Just "test", "foo.yaml")

    context "when file does not exist" $ do
      it "defaults directory to Nothing" $ do
        inTempDirectory $ do
          splitDirectory packageConfig (Just "test") `shouldReturn` (Nothing, "test")

    context "when directory does not exist" $ do
      it "defaults file name to package.yaml" $ do
        inTempDirectory $ do
          splitDirectory packageConfig (Just "test/") `shouldReturn` (Just "test", "package.yaml")
