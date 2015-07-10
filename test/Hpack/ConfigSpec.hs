{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Hpack.ConfigSpec (
  spec

, package
, executable
, library
) where

import           Helper

import           Data.Aeson.QQ
import           Data.Aeson.Types
import           Data.String.Interpolate.IsString
import           Control.Arrow
import           System.Directory (createDirectory)
import           Data.Yaml

import           Hpack.Util
import           Hpack.Config hiding (package)
import qualified Hpack.Config as Config

package :: Package
package = Config.package "foo" "0.0.0"

executable :: String -> String -> Executable
executable name main_ = Executable name main_ []

library :: Library
library = Library [] [] []

withPackage :: String -> IO () -> (([String], Package) -> Expectation) -> Expectation
withPackage content beforeAction expectation = withTempDirectory $ \dir_ -> do
  let dir = dir_ </> "foo"
  createDirectory dir
  writeFile (dir </> "package.yaml") content
  withCurrentDirectory dir beforeAction
  r <- readPackageConfig (dir </> "package.yaml")
  either expectationFailure expectation r

withPackageConfig :: String -> IO () -> (Package -> Expectation) -> Expectation
withPackageConfig content beforeAction expectation = withPackage content beforeAction (expectation . snd)

withPackageConfig_ :: String -> (Package -> Expectation) -> Expectation
withPackageConfig_ content = withPackageConfig content (return ())

withPackageWarnings :: String -> IO () -> ([String] -> Expectation) -> Expectation
withPackageWarnings content beforeAction expectation = withPackage content beforeAction (expectation . fst)

withPackageWarnings_ :: String -> ([String] -> Expectation) -> Expectation
withPackageWarnings_ content = withPackageWarnings content (return ())

data Dummy = Dummy
  deriving (Eq, Show)

instance FromJSON Dummy where
  parseJSON _ = return Dummy

spec :: Spec
spec = do
  describe "parseJSON" $ do
    context "when parsing Section" $ do
      it "accepts dependencies" $ do
        let input = [i|
              dependencies: hpack
              |]
        decodeEither input `shouldBe` Right (section Dummy){sectionDependencies = ["hpack"]}

      it "accepts conditionals" $ do
        let input = [i|
              when:
                condition: os(windows)
                dependencies: Win32
              |]
            conditionals = [(section $ Condition "os(windows)"){sectionDependencies = ["Win32"]}]
        decodeEither input `shouldBe` Right (section Dummy){sectionConditionals = conditionals}

    context "when parsing a Dependency" $ do
      it "accepts simple dependencies" $ do
        parseEither parseJSON "hpack" `shouldBe` Right (Dependency "hpack" Nothing)

      it "accepts git dependencies" $ do
        let value = [aesonQQ|{
              name: "hpack",
              git: "https://github.com/sol/hpack",
              ref: "master"
            }|]
            source = GitRef "https://github.com/sol/hpack" "master"
        parseEither parseJSON value `shouldBe` Right (Dependency "hpack" (Just source))

      it "accepts github dependencies" $ do
        let value = [aesonQQ|{
              name: "hpack",
              github: "sol/hpack",
              ref: "master"
            }|]
            source = GitRef "https://github.com/sol/hpack" "master"
        parseEither parseJSON value `shouldBe` Right (Dependency "hpack" (Just source))

      it "accepts local dependencies" $ do
        let value = [aesonQQ|{
              name: "hpack",
              path: "../hpack"
            }|]
            source = Local "../hpack"
        parseEither parseJSON value `shouldBe` Right (Dependency "hpack" (Just source))

      context "when parsing fails" $ do
        it "returns an error message" $ do
          let value = Number 23
          parseEither parseJSON value `shouldBe` (Left "Error in $: expected String or an Object, encountered Number" :: Either String Dependency)

        context "when ref is missing" $ do
          it "produces accurate error messages" $ do
            let value = [aesonQQ|{
                  name: "hpack",
                  git: "sol/hpack",
                  ef: "master"
                }|]
            parseEither parseJSON value `shouldBe` (Left "Error in $: key \"ref\" not present" :: Either String Dependency)

        context "when both git and github are missing" $ do
          it "produces accurate error messages" $ do
            let value = [aesonQQ|{
                  name: "hpack",
                  gi: "sol/hpack",
                  ref: "master"
                }|]
            parseEither parseJSON value `shouldBe` (Left "Error in $: neither key \"git\" nor key \"github\" present" :: Either String Dependency)

  describe "getModules" $ around withTempDirectory $ do
    it "returns Haskell modules in specified source directory" $ \dir -> do
      touch (dir </> "src/Foo.hs")
      touch (dir </> "src/Bar/Baz.hs")
      touch (dir </> "src/Setup.hs")
      getModules dir "src" >>= (`shouldMatchList` ["Foo", "Bar.Baz", "Setup"])

    context "when source directory is '.'" $ do
      it "ignores Setup" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Setup.hs")
        getModules dir  "." `shouldReturn` ["Foo"]

    context "when source directory is './.'" $ do
      it "ignores Setup" $ \dir -> do
        touch (dir </> "Foo.hs")
        touch (dir </> "Setup.hs")
        getModules dir  "./." `shouldReturn` ["Foo"]

  describe "determineModules" $ do
    it "adds the Paths_* module to the other-modules" $ do
      determineModules "foo" [] (Just $ List ["Foo"]) Nothing `shouldBe` (["Foo"], ["Paths_foo"])

    it "replaces dashes with underscores in Paths_*" $ do
      determineModules "foo-bar" [] (Just $ List ["Foo"]) Nothing `shouldBe` (["Foo"], ["Paths_foo_bar"])

    context "when the Paths_* module is part of the exposed-modules" $ do
      it "does not add the Paths_* module to the other-modules" $ do
        determineModules "foo" [] (Just $ List ["Foo", "Paths_foo"]) Nothing `shouldBe` (["Foo", "Paths_foo"], [])

  describe "readPackageConfig" $ do
    it "warns on unknown fields" $ do
      withPackageWarnings_ [i|
        bar: 23
        baz: 42
        |]
        (`shouldBe` [
          "Ignoring unknown field \"bar\" in package description"
        , "Ignoring unknown field \"baz\" in package description"
        ]
        )

    it "accepts name" $ do
      withPackageConfig_ [i|
        name: bar
        |]
        (packageName >>> (`shouldBe` "bar"))

    it "accepts version" $ do
      withPackageConfig_ [i|
        version: 0.1.0
        |]
        (packageVersion >>> (`shouldBe` "0.1.0"))

    it "accepts synopsis" $ do
      withPackageConfig_ [i|
        synopsis: some synopsis
        |]
        (packageSynopsis >>> (`shouldBe` Just "some synopsis"))

    it "accepts description" $ do
      withPackageConfig_ [i|
        description: some description
        |]
        (packageDescription >>> (`shouldBe` Just "some description"))

    it "accepts category" $ do
      withPackageConfig_ [i|
        category: Data
        |]
        (`shouldBe` package {packageCategory = Just "Data"})

    it "accepts author" $ do
      withPackageConfig_ [i|
        author: John Doe
        |]
        (`shouldBe` package {packageAuthor = ["John Doe"]})

    it "accepts maintainer" $ do
      withPackageConfig_ [i|
        maintainer: John Doe <john.doe@example.com>
        |]
        (`shouldBe` package {packageMaintainer = ["John Doe <john.doe@example.com>"]})

    it "accepts copyright" $ do
      withPackageConfig_ [i|
        copyright: (c) 2015 John Doe
        |]
        (`shouldBe` package {packageCopyright = ["(c) 2015 John Doe"]})

    it "accepts stability" $ do
      withPackageConfig_ [i|
        stability: experimental
        |]
        (packageStability >>> (`shouldBe` Just "experimental"))

    it "accepts homepage URL" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        homepage: https://example.com/
        |]
        (packageHomepage >>> (`shouldBe` Just "https://example.com/"))

    it "infers homepage URL from github" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        |]
        (packageHomepage >>> (`shouldBe` Just "https://github.com/hspec/hspec#readme"))

    it "omits homepage URL if it is null" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        homepage: null
        |]
        (packageHomepage >>> (`shouldBe` Nothing))

    it "accepts bug-reports URL" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        bug-reports: https://example.com/issues
        |]
        (packageBugReports >>> (`shouldBe` Just "https://example.com/issues"))

    it "infers bug-reports URL from github" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        |]
        (packageBugReports >>> (`shouldBe` Just "https://github.com/hspec/hspec/issues"))

    it "omits bug-reports URL if it is null" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        bug-reports: null
        |]
        (packageBugReports >>> (`shouldBe` Nothing))

    it "accepts license" $ do
      withPackageConfig_ [i|
        license: MIT
        |]
        (`shouldBe` package {packageLicense = Just "MIT"})

    it "infers license file" $ do
      withPackageConfig [i|
        name: foo
        |]
        (do
        touch "LICENSE"
        )
        (packageLicenseFile >>> (`shouldBe` Just "LICENSE"))

    it "accepts license file" $ do
      withPackageConfig_ [i|
        license-file: FOO
        |]
        (packageLicenseFile >>> (`shouldBe` Just "FOO"))

    it "accepts extra-source-files" $ do
      withPackageConfig [i|
        extra-source-files:
          - CHANGES.markdown
          - README.markdown
        |]
        (do
        touch "CHANGES.markdown"
        touch "README.markdown"
        )
        (packageExtraSourceFiles >>> (`shouldBe` ["CHANGES.markdown", "README.markdown"]))

    it "accepts data-files" $ do
      withPackageConfig [i|
        data-files:
          - data/**/*.html
        |]
        (do
        touch "data/foo/index.html"
        touch "data/bar/index.html"
        )
        (packageDataFiles >>> (`shouldMatchList` ["data/foo/index.html", "data/bar/index.html"]))

    it "accepts github" $ do
      withPackageConfig_ [i|
        github: hspec/hspec
        |]
        (packageSourceRepository >>> (`shouldBe` Just (SourceRepository "https://github.com/hspec/hspec" Nothing)))

    it "accepts third part of github URL as subdir" $ do
      withPackageConfig_ [i|
        github: hspec/hspec/hspec-core
        |]
        (packageSourceRepository >>> (`shouldBe` Just (SourceRepository "https://github.com/hspec/hspec" (Just "hspec-core"))))

    it "accepts CPP options" $ do
      withPackageConfig_ [i|
        cpp-options: -DFOO
        library:
          cpp-options: -DLIB

        executables:
          foo:
            main: Main.hs
            cpp-options: -DFOO


        tests:
          spec:
            main: Spec.hs
            cpp-options: -DTEST
        |]
        (`shouldBe` package {
          packageLibrary = Just (section library) {sectionCppOptions = ["-DFOO", "-DLIB"]}
        , packageExecutables = [(section $ executable "foo" "Main.hs") {sectionCppOptions = ["-DFOO", "-DFOO"]}]
        , packageTests = [(section $ executable "spec" "Spec.hs") {sectionCppOptions = ["-DFOO", "-DTEST"]}]
        }
        )

    context "when reading library section" $ do
      it "warns on unknown fields" $ do
        withPackageWarnings_ [i|
          library:
            bar: 23
            baz: 42
          |]
          (`shouldBe` [
            "Ignoring unknown field \"bar\" in library section"
          , "Ignoring unknown field \"baz\" in library section"
          ]
          )

      it "accepts source-dirs" $ do
        withPackageConfig_ [i|
          library:
            source-dirs:
              - foo
              - bar
          |]
          (packageLibrary >>> (`shouldBe` Just (section library) {sectionSourceDirs = ["foo", "bar"]}))

      it "accepts default-extensions" $ do
        withPackageConfig_ [i|
          library:
            default-extensions:
              - Foo
              - Bar
          |]
          (packageLibrary >>> (`shouldBe` Just (section library) {sectionDefaultExtensions = ["Foo", "Bar"]}))

      it "accepts global default-extensions" $ do
        withPackageConfig_ [i|
          default-extensions:
            - Foo
            - Bar
          library: {}
          |]
          (packageLibrary >>> (`shouldBe` Just (section library) {sectionDefaultExtensions = ["Foo", "Bar"]}))

      it "accepts global source-dirs" $ do
        withPackageConfig_ [i|
          source-dirs:
            - foo
            - bar
          library: {}
          |]
          (packageLibrary >>> (`shouldBe` Just (section library) {sectionSourceDirs = ["foo", "bar"]}))

      it "allows to specify exposed-modules" $ do
        withPackageConfig [i|
          library:
            source-dirs: src
            exposed-modules: Foo
          |]
          (do
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          )
          (packageLibrary >>> (`shouldBe` Just (section library{libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar", "Paths_foo"]}) {sectionSourceDirs = ["src"]}))

      it "allows to specify other-modules" $ do
        withPackageConfig [i|
          library:
            source-dirs: src
            other-modules: Bar
          |]
          (do
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          )
          (packageLibrary >>> (`shouldBe` Just (section library{libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}) {sectionSourceDirs = ["src"]}))

      it "allows to specify reexported-modules" $ do
        withPackageConfig_ [i|
          library:
            reexported-modules: Baz
          |]
          (packageLibrary >>> (`shouldBe` Just (section library{libraryReexportedModules = ["Baz"]})))

      it "allows to specify both exposed-modules and other-modules" $ do
        withPackageConfig [i|
          library:
            source-dirs: src
            exposed-modules: Foo
            other-modules: Bar
          |]
          (do
          touch "src/Baz.hs"
          )
          (packageLibrary >>> (`shouldBe` Just (section library{libraryExposedModules = ["Foo"], libraryOtherModules = ["Bar"]}) {sectionSourceDirs = ["src"]}))

      context "when neither exposed-modules nor other-modules are specified" $ do
        it "exposes all modules" $ do
          withPackageConfig [i|
            library:
              source-dirs: src
            |]
            (do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            )
            (packageLibrary >>> (`shouldBe` Just (section library{libraryExposedModules = ["Bar", "Foo"]}) {sectionSourceDirs = ["src"]}))

    context "when reading executable section" $ do
      it "warns on unknown fields" $ do
        withPackageWarnings_ [i|
          executables:
            foo:
              main: Main.hs
              bar: 42
              baz: 23
          |]
          (`shouldBe` [
            "Ignoring unknown field \"bar\" in executable section \"foo\""
          , "Ignoring unknown field \"baz\" in executable section \"foo\""
          ]
          )

      it "reads executable section" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
          |]
          (packageExecutables >>> (`shouldBe` [section $ executable "foo" "driver/Main.hs"]))

      it "accepts arbitrary entry points as main" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: Foo
          |]
          (packageExecutables >>> (`shouldBe` [
            (section $ executable "foo" "Foo.hs") {sectionGhcOptions = ["-main-is Foo"]}
          ]
          ))

      it "accepts source-dirs" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: Main.hs
              source-dirs:
                - foo
                - bar
          |]
          (packageExecutables >>> (`shouldBe` [(section $ executable "foo" "Main.hs") {sectionSourceDirs = ["foo", "bar"]}]))

      it "accepts global source-dirs" $ do
        withPackageConfig_ [i|
          source-dirs:
            - foo
            - bar
          executables:
            foo:
              main: Main.hs
          |]
          (packageExecutables >>> (`shouldBe` [(section $ executable "foo" "Main.hs") {sectionSourceDirs = ["foo", "bar"]}]))

      it "infers other-modules" $ do
        withPackageConfig [i|
          executables:
            foo:
              main: Main.hs
              source-dirs: src
          |]
          (do
          touch "src/Main.hs"
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          touch "src/Baz.lhs"
          )
          (map (executableOtherModules . sectionData) . packageExecutables >>> (`shouldBe` [["Bar", "Baz", "Foo"]]))

      it "allows to specify other-modules" $ do
        withPackageConfig [i|
          executables:
            foo:
              main: Main.hs
              source-dirs: src
              other-modules: Baz
          |]
          (do
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          )
          (map (executableOtherModules . sectionData) . packageExecutables >>> (`shouldBe` [["Baz"]]))

      it "accepts default-extensions" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
              default-extensions:
                - Foo
                - Bar
          |]
          (packageExecutables >>> (`shouldBe` [(section $ executable "foo" "driver/Main.hs") {sectionDefaultExtensions = ["Foo", "Bar"]}]))

      it "accepts global default-extensions" $ do
        withPackageConfig_ [i|
          default-extensions:
            - Foo
            - Bar
          executables:
            foo:
              main: driver/Main.hs
          |]
          (packageExecutables >>> (`shouldBe` [(section $ executable "foo" "driver/Main.hs") {sectionDefaultExtensions = ["Foo", "Bar"]}]))

      it "accepts GHC options" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
              ghc-options: -Wall
          |]
          (`shouldBe` package {packageExecutables = [(section $ executable "foo" "driver/Main.hs") {sectionGhcOptions = ["-Wall"]}]})

      it "accepts global GHC options" $ do
        withPackageConfig_ [i|
          ghc-options: -Wall
          executables:
            foo:
              main: driver/Main.hs
          |]
          (`shouldBe` package {packageExecutables = [(section $ executable "foo" "driver/Main.hs") {sectionGhcOptions = ["-Wall"]}]})

      it "accepts GHC profiling options" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
              ghc-prof-options: -fprof-auto
          |]
          (`shouldBe` package {packageExecutables = [(section $ executable "foo" "driver/Main.hs") {sectionGhcProfOptions = ["-fprof-auto"]}]})

      it "accepts global GHC profiling options" $ do
        withPackageConfig_ [i|
          ghc-prof-options: -fprof-auto
          executables:
            foo:
              main: driver/Main.hs
          |]
          (`shouldBe` package {packageExecutables = [(section $ executable "foo" "driver/Main.hs") {sectionGhcProfOptions = ["-fprof-auto"]}]})


    context "when reading test section" $ do
      it "warns on unknown fields" $ do
        withPackageWarnings_ [i|
          tests:
            foo:
              main: Main.hs
              bar: 42
              baz: 23
          |]
          (`shouldBe` [
            "Ignoring unknown field \"bar\" in test section \"foo\""
          , "Ignoring unknown field \"baz\" in test section \"foo\""
          ]
          )

      it "reads test section" $ do
        withPackageConfig_ [i|
          tests:
            spec:
              main: test/Spec.hs
          |]
          (`shouldBe` package {packageTests = [section $ executable "spec" "test/Spec.hs"]})

      it "accepts single dependency" $ do
        withPackageConfig_ [i|
          tests:
            spec:
              main: test/Spec.hs
              dependencies: hspec
          |]
          (`shouldBe` package {packageTests = [(section $ executable "spec" "test/Spec.hs") {sectionDependencies = ["hspec"]}]})

      it "accepts list of dependencies" $ do
        withPackageConfig_ [i|
          tests:
            spec:
              main: test/Spec.hs
              dependencies:
                - hspec
                - QuickCheck
          |]
          (`shouldBe` package {packageTests = [(section $ executable "spec" "test/Spec.hs") {sectionDependencies = ["hspec", "QuickCheck"]}]})

      context "when both global and section specific dependencies are specified" $ do
        it "combines dependencies" $ do
          withPackageConfig_ [i|
            dependencies:
              - base

            tests:
              spec:
                main: test/Spec.hs
                dependencies: hspec
            |]
            (`shouldBe` package {packageTests = [(section $ executable "spec" "test/Spec.hs") {sectionDependencies = ["base", "hspec"]}]})

    context "when a specified source directory does not exist" $ do
      it "warns" $ do
        withPackageWarnings [i|
          source-dirs:
            - some-dir
            - some-existing-dir
          library:
            source-dirs: some-lib-dir
          executables:
            main:
              main: Main.hs
              source-dirs: some-exec-dir
          tests:
            spec:
              main: Main.hs
              source-dirs: some-test-dir
          |]
          (do
          touch "some-existing-dir/foo"
          )
          (`shouldBe` [
            "Specified source-dir \"some-dir\" does not exist"
          , "Specified source-dir \"some-exec-dir\" does not exist"
          , "Specified source-dir \"some-lib-dir\" does not exist"
          , "Specified source-dir \"some-test-dir\" does not exist"
          ]
          )

    around withTempDirectory $ do
      context "when package.yaml can not be parsed" $ do
        it "returns an error" $ \dir -> do
          let file = dir </> "package.yaml"
          writeFile file [i|
            foo: bar
            foo baz
            |]
          readPackageConfig file `shouldReturn` Left (file ++ ":3:12: could not find expected ':' while scanning a simple key")

      context "when package.yaml is invalid" $ do
        it "returns an error" $ \dir -> do
          let file = dir </> "package.yaml"
          writeFile file [i|
            executables:
              foo:
                ain: driver/Main.hs
            |]
          readPackageConfig file `shouldReturn` Left (file ++ ": Error in $.executables.foo: failed to parse field executables: The key \"main\" was not found")

      context "when package.yaml does not exist" $ do
        it "returns an error" $ \dir -> do
          let file = dir </> "package.yaml"
          readPackageConfig file `shouldReturn` Left [i|#{file}: Yaml file not found: #{file}|]
