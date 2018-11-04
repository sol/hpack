{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hpack.ConfigSpec (
  spec

, package
, deps
, defaultInfo
) where

import           Helper
import           Data.Aeson.Config.FromValueSpec hiding (spec)

import           Data.String.Interpolate.IsString
import           Control.Arrow
import qualified GHC.Exts as Exts
import           System.Directory (createDirectory)
import           Data.Either
import qualified Data.Map.Lazy as Map
import           Control.Monad.Trans.Writer (runWriter)

import           Hpack.Syntax.Dependencies
import           Hpack.Syntax.DependencyVersion
import           Hpack.Syntax.BuildTools
import           Hpack.Config hiding (package)
import qualified Hpack.Config as Config

import           Data.Aeson.Config.Types
import           Data.Aeson.Config.FromValue


instance Exts.IsList (Maybe (List a)) where
  type Item (Maybe (List a)) = a
  fromList = Just . List
  toList = undefined

defaultInfo :: DependencyInfo
defaultInfo = DependencyInfo [] anyVersion

deps :: [String] -> Dependencies
deps = Dependencies . Map.fromList . map (flip (,) defaultInfo)

package :: Package
package = Config.package "foo" "0.0.0"

executable :: String -> Executable
executable main_ = Executable (Just main_) ["Paths_foo"] []

library :: Library
library = Library Nothing [] ["Paths_foo"] [] [] []

testDecodeOptions :: FilePath -> DecodeOptions
testDecodeOptions file = defaultDecodeOptions {decodeOptionsTarget = file, decodeOptionsUserDataDir = Just undefined}

withPackage :: HasCallStack => String -> IO () -> ((Package, [String]) -> Expectation) -> Expectation
withPackage content beforeAction expectation = withTempDirectory $ \dir_ -> do
  let dir = dir_ </> "foo"
  createDirectory dir
  writeFile (dir </> "package.yaml") content
  withCurrentDirectory dir beforeAction
  r <- readPackageConfig (testDecodeOptions $ dir </> "package.yaml")
  either expectationFailure (\ (DecodeResult p _ _ warnings) -> expectation (p, warnings)) r

withPackageConfig :: String -> IO () -> (Package -> Expectation) -> Expectation
withPackageConfig content beforeAction expectation = withPackage content beforeAction (expectation . fst)

withPackageConfig_ :: String -> (Package -> Expectation) -> Expectation
withPackageConfig_ content = withPackageConfig content (return ())

withPackageWarnings :: HasCallStack => String -> IO () -> ([String] -> Expectation) -> Expectation
withPackageWarnings content beforeAction expectation = withPackage content beforeAction (expectation . snd)

withPackageWarnings_ :: HasCallStack => String -> ([String] -> Expectation) -> Expectation
withPackageWarnings_ content = withPackageWarnings content (return ())

spec :: Spec
spec = do
  describe "pathsModuleFromPackageName" $ do
    it "replaces dashes with underscores in package name" $ do
      pathsModuleFromPackageName "foo-bar" `shouldBe` "Paths_foo_bar"

  describe "fromLibrarySectionInConditional" $ do
    let
      sect = LibrarySection {
        librarySectionExposed = Nothing
      , librarySectionExposedModules = Nothing
      , librarySectionGeneratedExposedModules = Nothing
      , librarySectionOtherModules = Nothing
      , librarySectionGeneratedOtherModules = Nothing
      , librarySectionReexportedModules = Nothing
      , librarySectionSignatures = Nothing
      }
      lib = Library {
        libraryExposed = Nothing
      , libraryExposedModules = []
      , libraryOtherModules = []
      , libraryGeneratedModules = []
      , libraryReexportedModules = []
      , librarySignatures = []
      }
      inferableModules = ["Foo", "Bar"]
      from = fromLibrarySectionInConditional inferableModules

    context "when inferring modules" $ do
      it "infers other-modules" $ do
        from sect `shouldBe` lib {libraryOtherModules = ["Foo", "Bar"]}

      context "with exposed-modules" $ do
        it "infers nothing" $ do
          from sect {librarySectionExposedModules = []} `shouldBe` lib

      context "with other-modules" $ do
        it "infers nothing" $ do
          from sect {librarySectionOtherModules = []} `shouldBe` lib

  describe "renamePackage" $ do
    it "renames a package" $ do
      renamePackage "bar" package `shouldBe` package {packageName = "bar"}

    it "renames dependencies on self" $ do
      let packageWithExecutable dependencies = package {packageExecutables = Map.fromList [("main", (section $ executable "Main.hs") {sectionDependencies = deps dependencies})]}
      renamePackage "bar" (packageWithExecutable ["foo"]) `shouldBe` (packageWithExecutable ["bar"]) {packageName = "bar"}

  describe "renameDependencies" $ do
    let sectionWithDeps dependencies = (section ()) {sectionDependencies = deps dependencies}

    it "renames dependencies" $ do
      renameDependencies "bar" "baz" (sectionWithDeps ["foo", "bar"]) `shouldBe` sectionWithDeps ["foo", "baz"]

    it "renames dependency in conditionals" $ do
      let sectionWithConditional dependencies = (section ()) {
              sectionConditionals = [
                Conditional {
                  conditionalCondition = "some condition"
                , conditionalThen = sectionWithDeps dependencies
                , conditionalElse = Just (sectionWithDeps dependencies)
                }
                ]
            }
      renameDependencies "bar" "baz" (sectionWithConditional ["foo", "bar"]) `shouldBe` sectionWithConditional ["foo", "baz"]

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

  describe "toBuildTool" $ do
    let toBuildTool_ name = runWriter $ toBuildTool "my-package" ["foo"] (name, anyVersion)
    context "with an UnqualifiedBuildTool" $ do
      context "when name does not match a local executable" $ do
        it "returns a BuildTool" $ do
          toBuildTool_ (UnqualifiedBuildTool "bar") `shouldBe` (Right (BuildTool "bar" "bar", anyVersion), [])

      context "when name matches a local executable" $ do
        it "returns a LocalBuildTool" $ do
          toBuildTool_ (UnqualifiedBuildTool "foo") `shouldBe` (Right (LocalBuildTool "foo", anyVersion), [])

      context "when name matches a legacy executable" $ do
        it "warns" $ do
          toBuildTool_ (UnqualifiedBuildTool "gtk2hsTypeGen") `shouldBe` (Right (BuildTool "gtk2hs-buildtools" "gtk2hsTypeGen", anyVersion), ["Usage of the unqualified build-tool name \"gtk2hsTypeGen\" is deprecated! Please use the qualified name \"gtk2hs-buildtools:gtk2hsTypeGen\" instead!"])

      context "when name matches a legacy system build tool" $ do
        it "warns" $ do
          toBuildTool_ (UnqualifiedBuildTool "ghc") `shouldBe` (Left ("ghc", AnyVersion), ["Listing \"ghc\" under build-tools is deperecated! Please list system executables under system-build-tools instead!"])

    context "with a QualifiedBuildTool" $ do
      context "when only package matches the current package" $ do
        it "returns a BuildTool" $ do
          toBuildTool_ (QualifiedBuildTool "my-package" "bar") `shouldBe` (Right (BuildTool "my-package" "bar", anyVersion), [])

      context "when only executable matches a local executable" $ do
        it "returns a BuildTool" $ do
          toBuildTool_ (QualifiedBuildTool "other-package" "foo") `shouldBe` (Right (BuildTool "other-package" "foo", anyVersion), [])

      context "when both package matches the current package and executable matches a local executable" $ do
        it "returns a LocalBuildTool" $ do
          toBuildTool_ (QualifiedBuildTool "my-package" "foo") `shouldBe` (Right (LocalBuildTool "foo", anyVersion), [])

  describe "readPackageConfig" $ do
    it "warns on missing name" $ do
      withPackageWarnings_ [i|
        {}
        |]
        (`shouldBe` [
          "Package name not specified, inferred \"foo\""
        ]
        )

    it "infers name" $ do
      withPackageConfig_ [i|
        {}
        |]
        (packageName >>> (`shouldBe` "foo"))

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
        (`shouldBe` package {packageAuthor = ["John Doe"]
                            ,packageMaintainer = ["John Doe"]})

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
        (packageLicenseFile >>> (`shouldBe` ["LICENSE"]))

    it "accepts license file" $ do
      withPackageConfig_ [i|
        license-file: FOO
        |]
        (packageLicenseFile >>> (`shouldBe` ["FOO"]))

    it "accepts list of license files" $ do
      withPackageConfig_ [i|
        license-file: [FOO, BAR]
        |]
        (packageLicenseFile >>> (`shouldBe` ["FOO", "BAR"]))

    it "accepts flags" $ do
      withPackageConfig_ [i|
        flags:
          integration-tests:
            description: Run the integration test suite
            manual: yes
            default: no
        |]
        (packageFlags >>> (`shouldBe` [Flag "integration-tests" (Just "Run the integration test suite") True False]))

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

    it "accepts arbitrary git URLs as source repository" $ do
      withPackageConfig_ [i|
        git: https://gitlab.com/gitlab-org/gitlab-ce.git
        |]
        (packageSourceRepository >>> (`shouldBe` Just (SourceRepository "https://gitlab.com/gitlab-org/gitlab-ce.git" Nothing)))

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
        , packageExecutables = Map.fromList [("foo", (section $ executable "Main.hs") {sectionCppOptions = ["-DFOO", "-DFOO"]})]
        , packageTests = Map.fromList [("spec", (section $ executable "Spec.hs") {sectionCppOptions = ["-DFOO", "-DTEST"]})]
        }
        )

    it "accepts cc-options" $ do
      withPackageConfig_ [i|
        cc-options: -Wall
        library:
          cc-options: -fLIB

        executables:
          foo:
            main: Main.hs
            cc-options: -O2


        tests:
          spec:
            main: Spec.hs
            cc-options: -O0
        |]
        (`shouldBe` package {
          packageLibrary = Just (section library) {sectionCcOptions = ["-Wall", "-fLIB"]}
        , packageExecutables = Map.fromList [("foo", (section $ executable "Main.hs") {sectionCcOptions = ["-Wall", "-O2"]})]
        , packageTests = Map.fromList [("spec", (section $ executable "Spec.hs") {sectionCcOptions = ["-Wall", "-O0"]})]
        }
        )

    it "accepts ghcjs-options" $ do
      withPackageConfig_ [i|
        ghcjs-options: -dedupe
        library:
          ghcjs-options: -ghcjs1

        executables:
          foo:
            main: Main.hs
            ghcjs-options: -ghcjs2


        tests:
          spec:
            main: Spec.hs
            ghcjs-options: -ghcjs3
        |]
        (`shouldBe` package {
          packageLibrary = Just (section library) {sectionGhcjsOptions = ["-dedupe", "-ghcjs1"]}
        , packageExecutables = Map.fromList [("foo", (section $ executable "Main.hs") {sectionGhcjsOptions = ["-dedupe", "-ghcjs2"]})]
        , packageTests = Map.fromList [("spec", (section $ executable "Spec.hs") {sectionGhcjsOptions = ["-dedupe", "-ghcjs3"]})]
        }
        )

    it "accepts ld-options" $ do
      withPackageConfig_ [i|
        library:
          ld-options: -static
        |]
        (`shouldBe` package {
          packageLibrary = Just (section library) {sectionLdOptions = ["-static"]}
        }
        )

    it "accepts buildable" $ do
      withPackageConfig_ [i|
        buildable: no
        library:
          buildable: yes

        executables:
          foo:
            main: Main.hs
        |]
        (`shouldBe` package {
          packageLibrary = Just (section library) {sectionBuildable = Just True}
        , packageExecutables = Map.fromList [("foo", (section $ executable "Main.hs") {sectionBuildable = Just False})]
        }
        )

    it "allows yaml merging and overriding fields" $ do
      withPackageConfig_ [i|
        _common: &common
          name: n1

        <<: *common
        name: n2
        |]
        (packageName >>> (`shouldBe` "n2"))

    context "when reading library section" $ do
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

      it "allows to specify exposed" $ do
        withPackageConfig_ [i|
          library:
            exposed: no
          |]
          (packageLibrary >>> (`shouldBe` Just (section library{libraryExposed = Just False})))

    context "when reading executable section" $ do
      it "reads executables section" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
          |]
          (packageExecutables >>> (`shouldBe` Map.fromList [("foo", section $ executable "driver/Main.hs")]))

      it "reads executable section" $ do
        withPackageConfig_ [i|
          executable:
            main: driver/Main.hs
          |]
          (packageExecutables >>> (`shouldBe` Map.fromList [("foo", section $ executable "driver/Main.hs")]))

      context "with both executable and executables" $ do
        it "gives executable precedence" $ do
          withPackageConfig_ [i|
            executable:
              main: driver/Main1.hs
            executables:
              foo2:
                main: driver/Main2.hs
            |]
            (packageExecutables >>> (`shouldBe` Map.fromList [("foo", section $ executable "driver/Main1.hs")]))

        it "warns" $ do
          withPackageWarnings_ [i|
            name: foo
            executable:
              main: driver/Main1.hs
            executables:
              foo2:
                main: driver/Main2.hs
            |]
            (`shouldBe` ["Ignoring field \"executables\" in favor of \"executable\""])

      it "accepts source-dirs" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: Main.hs
              source-dirs:
                - foo
                - bar
          |]
          (packageExecutables >>> (`shouldBe` Map.fromList [("foo", (section (executable "Main.hs") {executableOtherModules = ["Paths_foo"]}) {sectionSourceDirs = ["foo", "bar"]})]))

      it "accepts global source-dirs" $ do
        withPackageConfig_ [i|
          source-dirs:
            - foo
            - bar
          executables:
            foo:
              main: Main.hs
          |]
          (packageExecutables >>> (`shouldBe` Map.fromList [("foo", (section (executable "Main.hs") {executableOtherModules = ["Paths_foo"]}) {sectionSourceDirs = ["foo", "bar"]})]))

      it "accepts default-extensions" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
              default-extensions:
                - Foo
                - Bar
          |]
          (packageExecutables >>> (`shouldBe` Map.fromList [("foo", (section $ executable "driver/Main.hs") {sectionDefaultExtensions = ["Foo", "Bar"]})]))

      it "accepts global default-extensions" $ do
        withPackageConfig_ [i|
          default-extensions:
            - Foo
            - Bar
          executables:
            foo:
              main: driver/Main.hs
          |]
          (packageExecutables >>> (`shouldBe` Map.fromList [("foo", (section $ executable "driver/Main.hs") {sectionDefaultExtensions = ["Foo", "Bar"]})]))

      it "accepts GHC options" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
              ghc-options: -Wall
          |]
          (`shouldBe` package {packageExecutables = Map.fromList [("foo", (section $ executable "driver/Main.hs") {sectionGhcOptions = ["-Wall"]})]})

      it "accepts global GHC options" $ do
        withPackageConfig_ [i|
          ghc-options: -Wall
          executables:
            foo:
              main: driver/Main.hs
          |]
          (`shouldBe` package {packageExecutables = Map.fromList [("foo", (section $ executable "driver/Main.hs") {sectionGhcOptions = ["-Wall"]})]})

      it "accepts GHC profiling options" $ do
        withPackageConfig_ [i|
          executables:
            foo:
              main: driver/Main.hs
              ghc-prof-options: -fprof-auto
          |]
          (`shouldBe` package {packageExecutables = Map.fromList [("foo", (section $ executable "driver/Main.hs") {sectionGhcProfOptions = ["-fprof-auto"]})]})

      it "accepts global GHC profiling options" $ do
        withPackageConfig_ [i|
          ghc-prof-options: -fprof-auto
          executables:
            foo:
              main: driver/Main.hs
          |]
          (`shouldBe` package {packageExecutables = Map.fromList [("foo", (section $ executable "driver/Main.hs") {sectionGhcProfOptions = ["-fprof-auto"]})]})

    context "when reading test section" $ do
      it "reads test section" $ do
        withPackageConfig_ [i|
          tests:
            spec:
              main: test/Spec.hs
          |]
          (`shouldBe` package {packageTests = Map.fromList [("spec", section $ executable "test/Spec.hs")]})

    context "when a specified source directory does not exist" $ do
      it "warns" $ do
        withPackageWarnings [i|
          name: foo
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
          readPackageConfig (testDecodeOptions file) `shouldReturn` Left (file ++ ":3:12: could not find expected ':' while scanning a simple key")

      context "when package.yaml is invalid" $ do
        it "returns an error" $ \dir -> do
          let file = dir </> "package.yaml"
          writeFile file [i|
            - one
            - two
            |]
          readPackageConfig (testDecodeOptions file) >>= (`shouldSatisfy` isLeft)

      context "when package.yaml does not exist" $ do
        it "returns an error" $ \dir -> do
          let file = dir </> "package.yaml"
          readPackageConfig (testDecodeOptions file) `shouldReturn` Left [i|#{file}: Yaml file not found: #{file}|]

  describe "fromValue" $ do
    context "with Cond" $ do
      it "accepts Strings" $ do
        [yaml|
        os(windows)
        |] `shouldDecodeTo_` Cond "os(windows)"

      it "accepts True" $ do
        [yaml|
        yes
        |] `shouldDecodeTo_` Cond "true"

      it "accepts False" $ do
        [yaml|
        no
        |] `shouldDecodeTo_` Cond "false"

      it "rejects other values" $ do
        [yaml|
        23
        |] `shouldDecodeTo` (Left "Error while parsing $ - expected Boolean or String, encountered Number" :: Result Cond)

  describe "formatOrList" $ do
    it "formats a singleton list" $ do
      formatOrList ["foo"] `shouldBe` "foo"

    it "formats a 2-element list" $ do
      formatOrList ["foo", "bar"] `shouldBe` "foo or bar"

    it "formats an n-element list" $ do
      formatOrList ["foo", "bar", "baz"] `shouldBe` "foo, bar, or baz"
