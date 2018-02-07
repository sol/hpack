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
        parseOptions configFile [] `shouldReturn` Run (Options Verbose NoForce False configFile)

      it "includes target" $ do
        parseOptions configFile ["foo.yaml"] `shouldReturn` Run (Options Verbose NoForce False "foo.yaml")

      context "with superfluous arguments" $ do
        it "returns ParseError" $ do
          parseOptions configFile ["foo", "bar"] `shouldReturn` ParseError

      context "with --silent" $ do
        it "sets optionsVerbose to NoVerbose" $ do
          parseOptions configFile ["--silent"] `shouldReturn` Run (Options NoVerbose NoForce False configFile)

      context "with --force" $ do
        it "sets optionsForce to Force" $ do
          parseOptions configFile ["--force"] `shouldReturn` Run (Options Verbose Force False configFile)

      context "with -f" $ do
        it "sets optionsForce to Force" $ do
          parseOptions configFile ["-f"] `shouldReturn` Run (Options Verbose Force False configFile)

      context "with -" $ do
        it "sets optionsToStdout to True" $ do
          parseOptions configFile ["-"] `shouldReturn` Run (Options Verbose NoForce True configFile)

        it "rejects - for target" $ do
          parseOptions configFile ["-", "-"] `shouldReturn` ParseError

  describe "expandConfigFile" $ around_ inTempDirectory $ do
    let defaultConfigFile = "foo.yaml"
    context "when target is Nothing" $ do
      it "return default file" $ do
        expandConfigFile defaultConfigFile Nothing `shouldReturn` defaultConfigFile

    context "when target is a file" $ do
      it "return file" $ do
        let file = "foo/bar.yaml"
        touch file
        expandConfigFile defaultConfigFile (Just file) `shouldReturn` file

    context "when target is a directory" $ do
      it "appends default file" $ do
        touch "foo/.placeholder"
        expandConfigFile defaultConfigFile (Just "foo") `shouldReturn` "foo" </> defaultConfigFile

    context "when target file does not exist" $ do
      it "return target file" $ do
        expandConfigFile defaultConfigFile (Just "foo/bar") `shouldReturn` "foo/bar"

    context "when target directory does not exist" $ do
      it "appends default file" $ do
        expandConfigFile defaultConfigFile (Just "foo/") `shouldReturn` "foo" </> defaultConfigFile

    context "when target is the empty string" $ do
      it "return default file" $ do
        expandConfigFile defaultConfigFile (Just "") `shouldReturn` defaultConfigFile
