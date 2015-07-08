{-# LANGUAGE OverloadedStrings #-}
module Hpack.RunSpec (spec) where

import           Test.Hspec
import           Data.List

import           Hpack.ConfigSpec hiding (spec)
import           Hpack.Config
import           Hpack.Run

spec :: Spec
spec = do
  describe "renderPackage" $ do
    it "renders a package" $ do
      renderPackage 0 [] package `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns fields" $ do
      renderPackage 16 [] package `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes description" $ do
      renderPackage 0 [] package {packageDescription = Just "foo\n\nbar\n"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "description: foo"
        , "             ."
        , "             bar"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns description" $ do
      renderPackage 16 [] package {packageDescription = Just "foo\n\nbar\n"} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "description:    foo"
        , "                ."
        , "                bar"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes stability" $ do
      renderPackage 0 [] package {packageStability = Just "experimental"} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "stability: experimental"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "includes copyright holder" $ do
      renderPackage 0 [] package {packageCopyright = ["(c) 2015 Simon Hengel"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "copyright: (c) 2015 Simon Hengel"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        ]

    it "aligns copyright holders" $ do
      renderPackage 16 [] package {packageCopyright = ["(c) 2015 Foo", "(c) 2015 Bar"]} `shouldBe` unlines [
          "name:           foo"
        , "version:        0.0.0"
        , "copyright:      (c) 2015 Foo,"
        , "                (c) 2015 Bar"
        , "build-type:     Simple"
        , "cabal-version:  >= 1.10"
        ]

    it "includes extra-source-files" $ do
      renderPackage 0 [] package {packageExtraSourceFiles = ["foo", "bar"]} `shouldBe` unlines [
          "name: foo"
        , "version: 0.0.0"
        , "build-type: Simple"
        , "cabal-version: >= 1.10"
        , ""
        , "extra-source-files:"
        , "  foo"
        , "  bar"
        ]

    context "when given list of existing fields" $ do
      it "retains field order" $ do
        renderPackage 16 ["cabal-version", "version", "name", "build-type"] package `shouldBe` unlines [
            "cabal-version:  >= 1.10"
          , "version:        0.0.0"
          , "name:           foo"
          , "build-type:     Simple"
          ]

      it "uses default field order for new fields" $ do
        renderPackage 16 ["name", "version", "cabal-version"] package `shouldBe` unlines [
            "name:           foo"
          , "version:        0.0.0"
          , "build-type:     Simple"
          , "cabal-version:  >= 1.10"
          ]

    context "when rendering executable section" $ do
      it "includes dependencies" $ do
        renderPackage 0 [] package {packageExecutables = [(section $ executable "foo" "Main.hs") {sectionDependencies = ["foo", "bar", "foo", "baz"]}]} `shouldBe` unlines [
            "name: foo"
          , "version: 0.0.0"
          , "build-type: Simple"
          , "cabal-version: >= 1.10"
          , ""
          , "executable foo"
          , "  main-is: Main.hs"
          , "  build-depends:"
          , "      foo"
          , "    , bar"
          , "    , foo"
          , "    , baz"
          , "  default-language: Haskell2010"
          ]

      it "includes GHC options" $ do
        renderPackage 0 [] package {packageExecutables = [(section $ executable "foo" "Main.hs") {sectionGhcOptions = ["-Wall", "-Werror"]}]} `shouldBe` unlines [
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
      renderSourceRepository (SourceRepository "https://github.com/hspec/hspec" Nothing)
        `shouldBe` unlines [
            "source-repository head"
          , "  type: git"
          , "  location: https://github.com/hspec/hspec"
          ]

    it "renders source-repository with subdir" $ do
      renderSourceRepository (SourceRepository "https://github.com/hspec/hspec" (Just "hspec-core"))
        `shouldBe` unlines [
            "source-repository head"
          , "  type: git"
          , "  location: https://github.com/hspec/hspec"
          , "  subdir: hspec-core"
          ]
