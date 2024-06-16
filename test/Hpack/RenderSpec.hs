{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Hpack.RenderSpec (spec) where

import           Helper

import           Hpack.Syntax.DependencyVersion
import           Hpack.ConfigSpec hiding (spec)
import           Hpack.Config hiding (package)
import           Hpack.Render.Dsl
import           Hpack.Render

library :: Library
library = Library Nothing Nothing [] [] [] [] []

executable :: Section Executable
executable = (section $ Executable (Just "Main.hs") [] []) {
  sectionLanguage = Just $ Language "Haskell2010"
}

renderEmptySection :: Empty -> [Element]
renderEmptySection Empty = []

spec :: Spec
spec = do
  describe "renderPackageWith" $ do
    let renderPackage_ = renderPackageWith defaultRenderSettings 0 [] []
    it "renders a package" $ do
      renderPackage_ package `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        ]

    it "aligns fields" $ do
      renderPackageWith defaultRenderSettings 16 [] [] package `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "build-type:     Simple"
        ]

    it "includes description" $ do
      renderPackage_ package {packageDescription = Just "foo\n\nbar\n"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "description: foo"
        , "             ."
        , "             bar"
        , "build-type: Simple"
        ]

    it "aligns description" $ do
      renderPackageWith defaultRenderSettings 16 [] [] package {packageDescription = Just "foo\n\nbar\n"} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "description:    foo"
        , "                ."
        , "                bar"
        , "build-type:     Simple"
        ]

    it "includes stability" $ do
      renderPackage_ package {packageStability = Just "experimental"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "stability: experimental"
        , "build-type: Simple"
        ]

    it "includes license-file" $ do
      renderPackage_ package {packageLicenseFile = ["FOO"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "license-file: FOO"
        , "build-type: Simple"
        ]

    it "aligns license-files" $ do
      renderPackageWith defaultRenderSettings 16 [] [] package {packageLicenseFile = ["FOO", "BAR"]} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "license-files:  FOO,"
        , "                BAR"
        , "build-type:     Simple"
        ]

    it "includes copyright holder" $ do
      renderPackage_ package {packageCopyright = ["(c) 2015 Simon Hengel"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "copyright: (c) 2015 Simon Hengel"
        , "build-type: Simple"
        ]

    it "aligns copyright holders" $ do
      renderPackageWith defaultRenderSettings 16 [] [] package {packageCopyright = ["(c) 2015 Foo", "(c) 2015 Bar"]} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "copyright:      (c) 2015 Foo,"
        , "                (c) 2015 Bar"
        , "build-type:     Simple"
        ]

    it "includes extra-source-files" $ do
      renderPackage_ package {packageExtraSourceFiles = ["foo", "bar"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "extra-source-files:"
        , "    foo"
        , "    bar"
        ]

    it "includes buildable" $ do
      renderPackage_ package {packageLibrary = Just (section library){sectionBuildable = Just False}} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , ""
        , "library"
        , "  buildable: False"
        ]

    context "when given list of existing fields" $ do
      it "retains field order" $ do
        renderPackageWith defaultRenderSettings 16 ["version", "build-type", "name"] [] package `shouldBe` unlines [
            "version:        0.0.0"
          , "build-type:     Simple"
          , "name:           foo"
          ]

      it "uses default field order for new fields" $ do
        renderPackageWith defaultRenderSettings 16 [] [] package `shouldBe` unlines [
            "name:           foo"
          , "version:        0.0.0"
          , "build-type:     Simple"
          ]

      it "retains section field order" $ do
        renderPackageWith defaultRenderSettings 0 [] [("executable foo", ["default-language", "main-is", "ghc-options"])] package {packageExecutables = [("foo", executable {sectionGhcOptions = ["-Wall", "-Werror"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , ""
          , "executable foo"
          , "  default-language: Haskell2010"
          , "  main-is: Main.hs"
          , "  ghc-options: -Wall -Werror"
          ]

    context "when rendering executable section" $ do
      it "includes dependencies" $ do
        let dependencies = Dependencies
              [ ("foo", defaultInfo { dependencyInfoVersion = versionRange "== 0.1.0" })
              , ("bar", defaultInfo)
              ]
        renderPackage_ package {packageExecutables = [("foo", executable {sectionDependencies = dependencies})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  build-depends:"
          , "      bar"
          , "    , foo == 0.1.0"
          , "  default-language: Haskell2010"
          ]

      it "includes GHC options" $ do
        renderPackage_ package {packageExecutables = [("foo", executable {sectionGhcOptions = ["-Wall", "-Werror"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  ghc-options: -Wall -Werror"
          , "  default-language: Haskell2010"
          ]

      it "includes frameworks" $ do
        renderPackage_ package {packageExecutables = [("foo", executable {sectionFrameworks = ["foo", "bar"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  frameworks:"
          , "      foo"
          , "      bar"
          , "  default-language: Haskell2010"
          ]

      it "includes extra-framework-dirs" $ do
        renderPackage_ package {packageExecutables = [("foo", executable {sectionExtraFrameworksDirs = ["foo", "bar"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  extra-frameworks-dirs:"
          , "      foo"
          , "      bar"
          , "  default-language: Haskell2010"
          ]

      it "includes GHC profiling options" $ do
        renderPackage_ package {packageExecutables = [("foo", executable {sectionGhcProfOptions = ["-fprof-auto", "-rtsopts"]})]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
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

    it "conditionalises both build-depends and mixins" $ do
      let conditional = Conditional "os(windows)" (section Empty) {sectionDependencies = [("Win32", depInfo)]} Nothing
          depInfo = defaultInfo { dependencyInfoMixins = ["hiding (Blah)"] }
      render defaultRenderSettings 0 (renderConditional renderEmptySection conditional) `shouldBe` [
          "if os(windows)"
        , "  build-depends:"
        , "      Win32"
        , "  mixins:"
        , "      Win32 hiding (Blah)"
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
          , "    ./"
          ]

  describe "renderDependencies" $ do
    it "renders build-depends" $ do
      let deps_ =
            [ ("foo", DependencyInfo [] anyVersion)
            ]
      renderDependencies "build-depends" deps_ `shouldBe`
        [ Field "build-depends" $ CommaSeparatedList
            [ "foo"
            ]
        , Field "mixins" $ CommaSeparatedList []
        ]

    it "renders build-depends with versions" $ do
      let deps_ =
            [ ("foo", DependencyInfo [] (versionRange ">= 2 && < 3"))
            ]
      renderDependencies "build-depends" deps_ `shouldBe`
        [ Field "build-depends" $ CommaSeparatedList
            [ "foo >= 2 && < 3"
            ]
        , Field "mixins" $ CommaSeparatedList []
        ]

    it "renders mixins and build-depends for multiple modules" $ do
      let deps_ =
            [ ("foo", DependencyInfo ["(Foo as Foo1)"] anyVersion)
            , ("bar", DependencyInfo ["hiding (Spam)", "(Spam as Spam1) requires (Mod as Sig)"] anyVersion)
            ]
      renderDependencies "build-depends" deps_ `shouldBe`
        [ Field "build-depends" $ CommaSeparatedList
           [ "bar"
           , "foo"
           ]
        , Field "mixins" $ CommaSeparatedList
            [ "bar hiding (Spam)"
            , "bar (Spam as Spam1) requires (Mod as Sig)"
            , "foo (Foo as Foo1)"
            ]
        ]
