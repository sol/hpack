{-# LANGUAGE OverloadedStrings #-}
module Hpack.RunSpec (spec) where

import           Helper
import           Data.List.Compat
import qualified Data.Map.Lazy as Map

import           Hpack.ConfigSpec hiding (spec)
import           Hpack.Config hiding (package)
import           Hpack.Render
import           Hpack.Run

library :: Library
library = Library Nothing [] [] []

renderEmptySection :: Empty -> [Element]
renderEmptySection Empty = []

spec :: Spec
spec = do
  describe "renderPackage" $ do
    let renderPackage_ = renderPackage defaultRenderSettings 0 [] []
    it "renders a package" $ do
      renderPackage_ package `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns fields" $ do
      renderPackage defaultRenderSettings 16 [] [] package `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes description" $ do
      renderPackage_ package {packageDescription = Just "foo\n\nbar\n"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "description: foo"
        , "             ."
        , "             bar"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns description" $ do
      renderPackage defaultRenderSettings 16 [] [] package {packageDescription = Just "foo\n\nbar\n"} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "description:    foo"
        , "                ."
        , "                bar"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes stability" $ do
      renderPackage_ package {packageStability = Just "experimental"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "stability: experimental"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "includes license-file" $ do
      renderPackage_ package {packageLicenseFile = ["FOO"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "license-file: FOO"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns license-files" $ do
      renderPackage defaultRenderSettings 16 [] [] package {packageLicenseFile = ["FOO", "BAR"]} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "license-files:  FOO,"
        , "                BAR"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes copyright holder" $ do
      renderPackage_ package {packageCopyright = ["(c) 2015 Simon Hengel"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "copyright: (c) 2015 Simon Hengel"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns copyright holders" $ do
      renderPackage defaultRenderSettings 16 [] [] package {packageCopyright = ["(c) 2015 Foo", "(c) 2015 Bar"]} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "copyright:      (c) 2015 Foo,"
        , "                (c) 2015 Bar"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes extra-source-files" $ do
      renderPackage_ package {packageExtraSourceFiles = ["foo", "bar"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        , ""
        , "extra-source-files:"
        , "    foo"
        , "    bar"
        ]

    it "includes buildable" $ do
      renderPackage_ package {packageLibrary = Just (section library){sectionBuildable = Just False}} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        , ""
        , "library"
        , "  buildable: False"
        , "  default-language: Haskell2010"
        ]

    context "when rendering custom-setup section" $ do
      it "includes setup-depends" $ do
        let setup = CustomSetup
              { customSetupDependencies = deps ["foo", "bar"] }
        renderPackage_ package {packageCustomSetup = Just setup} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "custom-setup"
          , "  setup-depends:"
          , "      bar"
          , "    , foo"
          ]

    context "when rendering library section" $ do
      it "renders library section" $ do
        renderPackage_ package {packageLibrary = Just $ section library} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "library"
          , "  default-language: Haskell2010"
          ]

      it "includes exposed-modules" $ do
        renderPackage_ package {packageLibrary = Just (section library{libraryExposedModules = ["Foo"]})} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "library"
          , "  exposed-modules:"
          , "      Foo"
          , "  default-language: Haskell2010"
          ]

      it "includes other-modules" $ do
        renderPackage_ package {packageLibrary = Just (section library{libraryOtherModules = ["Bar"]})} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "library"
          , "  other-modules:"
          , "      Bar"
          , "  default-language: Haskell2010"
          ]

    context "when given list of existing fields" $ do
      it "retains field order" $ do
        renderPackage defaultRenderSettings 16 ["cabal-version", "version", "name", "build-type"] [] package `shouldBe` unlines [
            "cabal-version:  >= 1.10"
          , "version:        0.0.0"
          , "name:           foo"
          , "build-type:     Simple"
          ]

      it "uses default field order for new fields" $ do
        renderPackage defaultRenderSettings 16 ["name", "version", "cabal-version"] [] package `shouldBe` unlines [
            "name:           foo"
          , "version:        0.0.0"
          , "build-type:     Simple"
          , "cabal-version:  >= 1.10"
          ]

      it "retains section field order" $ do
        renderPackage defaultRenderSettings 0 [] [("executable foo", ["default-language", "main-is", "ghc-options"])] package {packageExecutables = Map.fromList [("foo", (section $ Executable (Just "Main.hs") []) {sectionGhcOptions = ["-Wall", "-Werror"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  default-language: Haskell2010"
          , "  main-is: Main.hs"
          , "  ghc-options: -Wall -Werror"
          ]


    context "when rendering executable section" $ do
      it "includes dependencies" $ do
        renderPackage_ package {packageExecutables = Map.fromList [("foo", (section $ Executable (Just "Main.hs") []) {sectionDependencies = Dependencies $ Map.fromList
        [("foo", VersionRange "== 0.1.0"), ("bar", AnyVersion)]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  build-depends:"
          , "      bar"
          , "    , foo == 0.1.0"
          , "  default-language: Haskell2010"
          ]

      it "includes GHC options" $ do
        renderPackage_ package {packageExecutables = Map.fromList [("foo", (section $ Executable (Just "Main.hs") []) {sectionGhcOptions = ["-Wall", "-Werror"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  ghc-options: -Wall -Werror"
          , "  default-language: Haskell2010"
          ]

      it "includes frameworks" $ do
        renderPackage_ package {packageExecutables = Map.fromList [("foo", (section $ Executable (Just "Main.hs") []) {sectionFrameworks = ["foo", "bar"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  frameworks:"
          , "      foo"
          , "      bar"
          , "  default-language: Haskell2010"
          ]

      it "includes extra-framework-dirs" $ do
        renderPackage_ package {packageExecutables = Map.fromList [("foo", (section $ Executable (Just "Main.hs") []) {sectionExtraFrameworksDirs = ["foo", "bar"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  extra-frameworks-dirs:"
          , "      foo"
          , "      bar"
          , "  default-language: Haskell2010"
          ]

      it "includes GHC profiling options" $ do
        renderPackage_ package {packageExecutables = Map.fromList [("foo", (section $ Executable (Just "Main.hs") []) {sectionGhcProfOptions = ["-fprof-auto", "-rtsopts"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  ghc-prof-options: -fprof-auto -rtsopts"
          , "  default-language: Haskell2010"
          ]

  describe "renderConditional" $ do
    it "renders conditionals" $ do
      let conditional = Conditional "os(windows)" (section Empty) {sectionDependencies = deps ["Win32"]} Nothing
      render defaultRenderSettings 0 (renderConditional renderEmptySection conditional) `shouldBe` [
          "if os(windows)"
        , "  build-depends:"
        , "      Win32"
        ]

    it "renders conditionals with else-branch" $ do
      let conditional = Conditional "os(windows)" (section Empty) {sectionDependencies = deps ["Win32"]} (Just $ (section Empty) {sectionDependencies = deps ["unix"]})
      render defaultRenderSettings 0 (renderConditional renderEmptySection conditional) `shouldBe` [
          "if os(windows)"
        , "  build-depends:"
        , "      Win32"
        , "else"
        , "  build-depends:"
        , "      unix"
        ]

    it "renders nested conditionals" $ do
      let conditional = Conditional "arch(i386)" (section Empty) {sectionGhcOptions = ["-threaded"], sectionConditionals = [innerConditional]} Nothing
          innerConditional = Conditional "os(windows)" (section Empty) {sectionDependencies = deps ["Win32"]} Nothing
      render defaultRenderSettings 0 (renderConditional renderEmptySection conditional) `shouldBe` [
          "if arch(i386)"
        , "  ghc-options: -threaded"
        , "  if os(windows)"
        , "    build-depends:"
        , "        Win32"
        ]

  describe "renderFlag" $ do
    it "renders flags" $ do
      let flag = (Flag "foo" (Just "some flag") True False)
      render defaultRenderSettings 0 (renderFlag flag) `shouldBe` [
          "flag foo"
        , "  description: some flag"
        , "  manual: True"
        , "  default: False"
        ]

  describe "formatDescription" $ do
    it "formats description" $ do
      let description = unlines [
              "foo"
            , "bar"
            ]
      "description: " ++ formatDescription 0 description `shouldBe` intercalate "\n" [
          "description: foo"
        , "             bar"
        ]

    it "takes specified alignment into account" $ do
      let description = unlines [
              "foo"
            , "bar"
            , "baz"
            ]
      "description:   " ++ formatDescription 15 description `shouldBe` intercalate "\n" [
          "description:   foo"
        , "               bar"
        , "               baz"
        ]

    it "formats empty lines" $ do
      let description = unlines [
              "foo"
            , "   "
            , "bar"
            ]
      "description: " ++ formatDescription 0 description `shouldBe` intercalate "\n" [
          "description: foo"
        , "             ."
        , "             bar"
        ]

  describe "renderSourceRepository" $ do
    it "renders source-repository without subdir correctly" $ do
      let repository = SourceRepository "https://github.com/hspec/hspec" Nothing
      (render defaultRenderSettings 0 $ renderSourceRepository repository)
        `shouldBe` [
            "source-repository head"
          , "  type: git"
          , "  location: https://github.com/hspec/hspec"
          ]

    it "renders source-repository with subdir" $ do
      let repository = SourceRepository "https://github.com/hspec/hspec" (Just "hspec-core")
      (render defaultRenderSettings 0 $ renderSourceRepository repository)
        `shouldBe` [
            "source-repository head"
          , "  type: git"
          , "  location: https://github.com/hspec/hspec"
          , "  subdir: hspec-core"
          ]

  describe "renderDirectories" $ do
    it "replaces . with ./. (for compatibility with cabal syntax)" $ do
      (render defaultRenderSettings 0 $ renderDirectories "name" ["."])
        `shouldBe` [
            "name:"
          , "    ./."
          ]
