module Hpack.OptionsSpec (spec) where

import           Helper

import           Hpack.Options

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let defaultTarget = "package.yaml"
    context "with --help" $ do
      it "returns Help" $ do
        parseOptions defaultTarget ["--help"] `shouldReturn` Help

    context "with --version" $ do
      it "returns PrintVersion" $ do
        parseOptions defaultTarget ["--version"] `shouldReturn` PrintVersion

    context "by default" $ do
      it "returns Run" $ do
        parseOptions defaultTarget [] `shouldReturn` Run (ParseOptions Verbose NoForce Nothing False defaultTarget MinimizeDiffs)

      it "includes target" $ do
        parseOptions defaultTarget ["foo.yaml"] `shouldReturn` Run (ParseOptions Verbose NoForce Nothing False "foo.yaml" MinimizeDiffs)

      context "with superfluous arguments" $ do
        it "returns ParseError" $ do
          parseOptions defaultTarget ["foo", "bar"] `shouldReturn` ParseError

      context "with --silent" $ do
        it "sets optionsVerbose to NoVerbose" $ do
          parseOptions defaultTarget ["--silent"] `shouldReturn` Run (ParseOptions NoVerbose NoForce Nothing False defaultTarget MinimizeDiffs)

      context "with --force" $ do
        it "sets optionsForce to Force" $ do
          parseOptions defaultTarget ["--force"] `shouldReturn` Run (ParseOptions Verbose Force Nothing False defaultTarget MinimizeDiffs)

      context "with -f" $ do
        it "sets optionsForce to Force" $ do
          parseOptions defaultTarget ["-f"] `shouldReturn` Run (ParseOptions Verbose Force Nothing False defaultTarget MinimizeDiffs)

      context "when determining parseOptionsHash" $ do

        it "assumes True on --hash" $ do
          parseOptions defaultTarget ["--hash"] `shouldReturn` Run (ParseOptions Verbose NoForce (Just True) False defaultTarget MinimizeDiffs)

        it "assumes False on --no-hash" $ do
          parseOptions defaultTarget ["--no-hash"] `shouldReturn` Run (ParseOptions Verbose NoForce (Just False) False defaultTarget MinimizeDiffs)

        it "gives last occurrence precedence" $ do
          parseOptions defaultTarget ["--no-hash", "--hash"] `shouldReturn` Run (ParseOptions Verbose NoForce (Just True) False defaultTarget MinimizeDiffs)
          parseOptions defaultTarget ["--hash", "--no-hash"] `shouldReturn` Run (ParseOptions Verbose NoForce (Just False) False defaultTarget MinimizeDiffs)

      context "with -" $ do
        it "sets optionsToStdout to True, implies Force and NoVerbose" $ do
          parseOptions defaultTarget ["-"] `shouldReturn` Run (ParseOptions NoVerbose Force Nothing True defaultTarget MinimizeDiffs)

        it "rejects - for target" $ do
          parseOptions defaultTarget ["-", "-"] `shouldReturn` ParseError

  describe "expandTarget" $ around_ inTempDirectory $ do
    let defaultTarget = "foo.yaml"
    context "when target is Nothing" $ do
      it "return default file" $ do
        expandTarget defaultTarget Nothing `shouldReturn` defaultTarget

    context "when target is a file" $ do
      it "return file" $ do
        let file = "foo/bar.yaml"
        touch file
        expandTarget defaultTarget (Just file) `shouldReturn` file

    context "when target is a directory" $ do
      it "appends default file" $ do
        touch "foo/.placeholder"
        expandTarget defaultTarget (Just "foo") `shouldReturn` "foo" </> defaultTarget

    context "when target file does not exist" $ do
      it "return target file" $ do
        expandTarget defaultTarget (Just "foo/bar") `shouldReturn` "foo/bar"

    context "when target directory does not exist" $ do
      it "appends default file" $ do
        expandTarget defaultTarget (Just "foo/") `shouldReturn` ("foo/" ++ defaultTarget)

    context "when target is the empty string" $ do
      it "return default file" $ do
        expandTarget defaultTarget (Just "") `shouldReturn` defaultTarget
