{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module EndToEndSpec (spec) where

import           Helper

import           System.Exit
import           Data.Maybe
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util

import qualified Hpack.Run as Hpack
import           Hpack.Config (packageConfig, readPackageConfig)
import           Hpack.FormattingHints (FormattingHints(..), sniffFormattingHints)

spec :: Spec
spec = around_ (inTempDirectoryNamed "foo") $ do
  describe "hpack" $ do
    describe "dependencies" $ do
      it "accepts single dependency" $ do
        [i|
        executable:
          dependencies: base
        |] `shouldRenderTo` executable "foo" [i|
        build-depends:
            base
        |]

      it "accepts list of dependencies" $ do
        [i|
        executable:
          dependencies:
            - base
            - transformers
        |] `shouldRenderTo` executable "foo" [i|
        build-depends:
            base
          , transformers
        |]

      context "with both global and section specific dependencies" $ do
        it "combines dependencies" $ do
          [i|
          dependencies:
            - base
          executable:
            dependencies: hspec
          |] `shouldRenderTo` executable "foo" [i|
          build-depends:
              base
            , hspec
          |]

        it "gives section specific dependencies precedence" $ do
          [i|
          dependencies:
            - base
          executable:
            dependencies: base >= 2
          |] `shouldRenderTo` executable "foo" [i|
          build-depends:
              base >=2
          |]

    describe "include-dirs" $ do
      it "accepts include-dirs" $ do
        [i|
        include-dirs:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        include-dirs:
            foo
            bar
        |]

    describe "install-includes" $ do
      it "accepts install-includes" $ do
        [i|
        install-includes:
          - foo.h
          - bar.h
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        install-includes:
            foo.h
            bar.h
        |]

    describe "js-sources" $ before_ (touch "foo.js" >> touch "jsbits/bar.js") $ do
      it "accepts js-sources" $ do
        [i|
        executable:
          js-sources:
            - foo.js
            - jsbits/*.js
        |] `shouldRenderTo` executable "foo" [i|
        js-sources:
            foo.js
            jsbits/bar.js
        |]

      it "accepts global js-sources" $ do
        [i|
        js-sources:
          - foo.js
          - jsbits/*.js
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        js-sources:
            foo.js
            jsbits/bar.js
        |]
    describe "extra-lib-dirs" $ do
      it "accepts extra-lib-dirs" $ do
        [i|
        extra-lib-dirs:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        extra-lib-dirs:
            foo
            bar
        |]

    describe "extra-libraries" $ do
      it "accepts extra-libraries" $ do
        [i|
        extra-libraries:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        extra-libraries:
            foo
            bar
        |]

    describe "extra-frameworks-dirs" $ do
      it "accepts extra-frameworks-dirs" $ do
        [i|
        extra-frameworks-dirs:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        extra-frameworks-dirs:
            foo
            bar
        |]

    describe "frameworks" $ do
      it "accepts frameworks" $ do
        [i|
        frameworks:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        frameworks:
            foo
            bar
        |]

    describe "c-sources" $ before_ (touch "cbits/foo.c" >> touch "cbits/bar.c") $ do
      context "with internal-libraries" $ do
        it "warns when a glob pattern does not match any files" $ do
          [i|
          name: foo
          internal-libraries:
            bar:
              c-sources: foo/*.c
          |] `shouldWarn` pure "Specified pattern \"foo/*.c\" for c-sources does not match any files"

      context "with library" $ do
        it "accepts global c-sources" $ do
          [i|
          c-sources: cbits/*.c
          library: {}
          |] `shouldRenderTo` library [i|
          other-modules:
              Paths_foo
          c-sources:
              cbits/bar.c
              cbits/foo.c
          |]

        it "accepts c-sources" $ do
          [i|
          library:
            c-sources: cbits/*.c
          |] `shouldRenderTo` library [i|
          other-modules:
              Paths_foo
          c-sources:
              cbits/bar.c
              cbits/foo.c
          |]

        it "accepts c-sources in conditional" $ do
          [i|
          library:
            when:
              condition: os(windows)
              c-sources: cbits/*.c
          |] `shouldRenderTo` library [i|
          other-modules:
              Paths_foo
          if os(windows)
            c-sources:
                cbits/bar.c
                cbits/foo.c
          |]

      context "with executables" $ do
        it "accepts global c-sources" $ do
          [i|
          c-sources: cbits/*.c
          executables:
            foo: {}
          |] `shouldRenderTo` executable "foo" [i|
          c-sources:
              cbits/bar.c
              cbits/foo.c
          |]

        it "accepts c-sources" $ do
          [i|
          executables:
            foo:
              c-sources: cbits/*.c
          |] `shouldRenderTo` executable "foo" [i|
          c-sources:
              cbits/bar.c
              cbits/foo.c
          |]

    context "with custom-setup" $ do
      it "warns on unknown fields" $ do
        [i|
        name: foo
        custom-setup:
          foo: 1
          bar: 2
        |] `shouldWarn` [
            "Ignoring unknown field \"bar\" in custom-setup section"
          , "Ignoring unknown field \"foo\" in custom-setup section"
          ]

      it "accepts dependencies" $ do
        [i|
        custom-setup:
          dependencies:
            - base
        |] `shouldRenderTo` customSetup [i|
        setup-depends:
            base
        |]

      it "leaves build-type alone, if it exists" $ do
        [i|
        build-type: Make
        custom-setup:
          dependencies:
            - base
        |] `shouldRenderTo` (customSetup [i|
        setup-depends:
            base
        |]) {packageBuildType = "Make"}

    context "with library" $ do
      it "accepts reexported-modules" $ do
        [i|
        library:
          reexported-modules: Baz
        |] `shouldRenderTo` (library [i|
        reexported-modules:
            Baz
        other-modules:
            Paths_foo
        |]) {packageCabalVersion = ">= 1.22"}

      context "when inferring modules" $ do
        context "with exposed-modules" $ do
          it "infers other-modules" $ do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            [i|
            library:
              source-dirs: src
              exposed-modules: Foo
            |] `shouldRenderTo` library [i|
            hs-source-dirs:
                src
            exposed-modules:
                Foo
            other-modules:
                Bar
                Paths_foo
            |]

        context "with other-modules" $ do
          it "infers exposed-modules" $ do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            [i|
            library:
              source-dirs: src
              other-modules: Bar
            |] `shouldRenderTo` library [i|
            hs-source-dirs:
                src
            exposed-modules:
                Foo
            other-modules:
                Bar
            |]

        context "with both exposed-modules and other-modules" $ do
          it "doesn't infer any modules" $ do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            [i|
            library:
              source-dirs: src
              exposed-modules: Foo
              other-modules: Bar
            |] `shouldRenderTo` library [i|
            hs-source-dirs:
                src
            exposed-modules:
                Foo
            other-modules:
                Bar
            |]

        context "with neither exposed-modules nor other-modules" $ do
          it "infers exposed-modules" $ do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            [i|
            library:
              source-dirs: src
            |] `shouldRenderTo` library [i|
            hs-source-dirs:
                src
            exposed-modules:
                Bar
                Foo
            other-modules:
                Paths_foo
            |]

        context "with a conditional" $ do
          it "doesn't infer any modules mentioned in that conditional" $ do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            [i|
            library:
              source-dirs: src
              when:
                condition: os(windows)
                exposed-modules: Foo
            |] `shouldRenderTo` library [i|
            hs-source-dirs:
                src
            if os(windows)
              exposed-modules:
                  Foo
            exposed-modules:
                Bar
            other-modules:
                Paths_foo
            |]

          context "with a source-dir inside the conditional" $ do
            it "infers other-modules" $ do
              touch "windows/Foo.hs"
              touch "windows/Bar.hs"
              [i|
              library:
                when:
                  condition: os(windows)
                  source-dirs: windows
                  when:
                    condition: foo
                    exposed-modules: Bar
              |] `shouldRenderTo` library [i|
              other-modules:
                  Paths_foo
              if os(windows)
                other-modules:
                    Foo
                hs-source-dirs:
                    windows
                if foo
                  exposed-modules:
                      Bar
              |]

    context "with internal-libraries" $ do
      it "accepts internal-libraries" $ do
        touch "src/Foo.hs"
        [i|
        internal-libraries:
          bar:
            source-dirs: src
          |] `shouldRenderTo` internalLibrary "bar" [i|
          exposed-modules:
              Foo
          other-modules:
              Paths_foo
          hs-source-dirs:
              src
          |]

      it "warns on unknown fields" $ do
        [i|
        name: foo
        internal-libraries:
          bar:
            baz: 42
        |] `shouldWarn` pure "Ignoring unknown field \"baz\" in internal-libraries section \"bar\""

      it "warns on missing source-dirs" $ do
        [i|
        name: foo
        internal-libraries:
          bar:
            source-dirs: src
        |] `shouldWarn` pure "Specified source-dir \"src\" does not exist"

    context "with executables" $ do
      it "accepts arbitrary entry points as main" $ do
        touch "src/Foo.hs"
        touch "src/Bar.hs"
        [i|
        executables:
          foo:
            source-dirs: src
            main: Foo
        |] `shouldRenderTo` executable "foo" [i|
        main-is: Foo.hs
        ghc-options: -main-is Foo
        hs-source-dirs:
            src
        other-modules:
            Bar
            Paths_foo
        |]

      context "when inferring modules" $ do
        it "infers other-modules" $ do
          touch "src/Main.hs"
          touch "src/Foo.hs"
          [i|
          executables:
            foo:
              main: Main.hs
              source-dirs: src
          |] `shouldRenderTo` executable "foo" [i|
            main-is: Main.hs
            hs-source-dirs:
                src
            other-modules:
                Foo
                Paths_foo
          |]

        it "allows to specify other-modules" $ do
          touch "src/Foo.hs"
          touch "src/Bar.hs"
          [i|
          executables:
            foo:
              main: Main.hs
              source-dirs: src
              other-modules: Baz
          |] `shouldRenderTo` executable "foo" [i|
            main-is: Main.hs
            hs-source-dirs:
                src
            other-modules:
                Baz
          |]
        context "with conditional" $ do
          it "doesn't infer any modules mentioned in that conditional" $ do
            touch "src/Foo.hs"
            touch "src/Bar.hs"
            [i|
            executables:
              foo:
                source-dirs: src
                when:
                  condition: os(windows)
                  other-modules: Foo
            |] `shouldRenderTo` executable "foo" [i|
            other-modules:
                Bar
                Paths_foo
            hs-source-dirs:
                src
            if os(windows)
              other-modules:
                  Foo
            |]

          it "infers other-modules" $ do
            touch "src/Foo.hs"
            touch "windows/Bar.hs"
            [i|
            executables:
              foo:
                source-dirs: src
                when:
                  condition: os(windows)
                  source-dirs: windows
            |] `shouldRenderTo` executable "foo" [i|
            other-modules:
                Foo
                Paths_foo
            hs-source-dirs:
                src
            if os(windows)
              other-modules:
                  Bar
              hs-source-dirs:
                  windows
            |]

      context "with conditional" $ do
        it "does not apply global options" $ do
          -- related bug: https://github.com/sol/hpack/issues/214
          [i|
          ghc-options: -Wall
          executables:
            foo:
              when:
                condition: os(windows)
                main: Foo.hs
          |] `shouldRenderTo` executable "foo" [i|
          ghc-options: -Wall
          if os(windows)
            main-is: Foo.hs
          |]

        it "accepts executable-specific fields" $ do
          [i|
          executables:
            foo:
              when:
                condition: os(windows)
                main: Foo
          |] `shouldRenderTo` executable "foo" [i|
          if os(windows)
            main-is: Foo.hs
            ghc-options: -main-is Foo
          |]

    describe "when" $ do
      it "accepts conditionals" $ do
        [i|
        when:
          condition: os(windows)
          dependencies: Win32
        executable: {}
        |] `shouldRenderTo` executable "foo" [i|
        if os(windows)
          build-depends:
              Win32
        |]
      it "warns on unknown fields" $ do
        [i|
        name: foo
        foo: 23
        when:
          - condition: os(windows)
            bar: 23
            when:
              condition: os(windows)
              bar2: 23
          - condition: os(windows)
            baz: 23
        |] `shouldWarn` [
            "Ignoring unknown field \"foo\" in package description"
          , "Ignoring unknown field \"bar\" in package description"
          , "Ignoring unknown field \"bar2\" in package description"
          , "Ignoring unknown field \"baz\" in package description"
          ]

      context "when parsing conditionals with else-branch" $ do
        it "accepts conditionals with else-branch" $ do
          [i|
          when:
            condition: os(windows)
            then:
              dependencies: Win32
            else:
              dependencies: unix
          executable: {}
          |] `shouldRenderTo` executable "foo" [i|
          if os(windows)
            build-depends:
                Win32
          else
            build-depends:
                unix
          |]

        it "rejects invalid conditionals" $ do
          [i|
          when:
            condition: os(windows)
            then:
              dependencies: Win32
            else: null
          |] `shouldFailWith` "package.yaml: Error in $.when.else: expected record (:*:), encountered Null"

        it "warns on unknown fields" $ do
          [i|
          name: foo
          when:
            condition: os(windows)
            foo: null
            then:
              bar: null
            else:
              when:
                condition: os(windows)
                then: {}
                else:
                  baz: null
          |] `shouldWarn` [
              "Ignoring unknown field \"foo\" in package description"
            , "Ignoring unknown field \"bar\" in package description"
            , "Ignoring unknown field \"baz\" in package description"
            ]

run :: FilePath -> String -> IO ([String], String)
run c old = run_ c old >>= either die return

run_ :: FilePath -> String -> IO (Either String ([String], String))
run_ c old = do
  mPackage <- readPackageConfig c
  return $ case mPackage of
    Right (warnings, pkg) ->
      let
        FormattingHints{..} = sniffFormattingHints old
        alignment = fromMaybe 0 formattingHintsAlignment
        settings = formattingHintsRenderSettings
        output = Hpack.renderPackage settings alignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder pkg
      in
        Right (warnings, output)
    Left err -> Left err

newtype PlainString = PlainString String
  deriving Eq

instance Show PlainString where
  show (PlainString xs) = xs

shouldRenderTo :: HasCallStack => String -> Package -> Expectation
shouldRenderTo input p = do
  writeFile packageConfig input
  (_ , output) <- run packageConfig expected
  PlainString (dropEmptyLines output) `shouldBe` PlainString expected
  where
    expected = dropEmptyLines (renderPackage p)
    dropEmptyLines = unlines . filter (not . null) . lines

shouldWarn :: HasCallStack => String -> [String] -> Expectation
shouldWarn input expected = do
  writeFile packageConfig input
  (warnings, _) <- run packageConfig ""
  sort warnings `shouldBe` sort expected

shouldFailWith :: HasCallStack => String -> String -> Expectation
shouldFailWith input expected = do
  writeFile packageConfig input
  run_ packageConfig "" `shouldReturn` Left expected

customSetup :: String -> Package
customSetup a = (package content) {packageCabalVersion = ">= 1.24", packageBuildType = "Custom"}
  where
    content = [i|
custom-setup
#{indentBy 2 $ unindent a}
|]

library :: String -> Package
library l = package content
  where
    content = [i|
library
#{indentBy 2 $ unindent l}
  default-language: Haskell2010
|]

internalLibrary :: String -> String -> Package
internalLibrary name e = (package content) {packageCabalVersion = ">= 2.0"}
  where
    content = [i|
library #{name}
#{indentBy 2 $ unindent e}
  default-language: Haskell2010
|]

executable :: String -> String -> Package
executable name e = package content
  where
    content = [i|
executable #{name}
#{indentBy 2 $ unindent e}
  default-language: Haskell2010
|]

package :: String -> Package
package = Package "foo" "Simple" ">= 1.10"

data Package = Package {
  packageName :: String
, packageBuildType :: String
, packageCabalVersion :: String
, packageContent :: String
}

renderPackage :: Package -> String
renderPackage Package{..} = unindent [i|
name: #{packageName}
version: 0.0.0
build-type: #{packageBuildType}
cabal-version: #{packageCabalVersion}

#{unindent packageContent}
|]

indentBy :: Int -> String -> String
indentBy n = unlines . map (replicate n ' ' ++) . lines
