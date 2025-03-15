{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
module EndToEndSpec (spec) where

import           Prelude hiding (writeFile)
import qualified Prelude

import           Helper
import           Test.HUnit

import           System.Directory (canonicalizePath)
import           Data.Maybe
import           Data.List
import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           Data.Version (showVersion)

import qualified Hpack.Render as Hpack
import           Hpack.Config (packageConfig, readPackageConfig, DecodeOptions(..), defaultDecodeOptions, DecodeResult(..))
import           Hpack.Render.Hints (FormattingHints(..), sniffFormattingHints)

import qualified Paths_hpack as Hpack (version)

writeFile :: FilePath -> String -> IO ()
writeFile file c = touch file >> Prelude.writeFile file c

spec :: Spec
spec = around_ (inTempDirectoryNamed "my-package") $ do
  describe "hpack" $ do
    it "ignores fields that start with an underscore" $ do
      [i|
      _foo:
        bar: 23
      library: {}
      |] `shouldRenderTo` library_ [i|
      |]
    it "warns on duplicate fields" $ do
      [i|
      name: foo
      name: foo
      |] `shouldWarn` [
          "package.yaml: Duplicate field $.name"
        ]

    describe "tested-with" $ do
      it "accepts a string" $ do
        [i|
        tested-with: GHC == 7.0.4
        |] `shouldRenderTo` package [i|
        tested-with:
            GHC == 7.0.4
        |]

      it "accepts a list" $ do
        [i|
        tested-with:
        - GHC == 7.0.4
        - GHC == 7.2.2
        - GHC == 7.4.2
        |] `shouldRenderTo` package [i|
        tested-with:
            GHC == 7.0.4
          , GHC == 7.2.2
          , GHC == 7.4.2
        |]

    describe "handling of Paths_ module" $ do
      it "adds Paths_ to other-modules" $ do
        [i|
        library: {}
        |] `shouldRenderTo` library [i|
        other-modules:
            Paths_my_package
        default-language: Haskell2010
        |]

      context "when spec-version is >= 0.36.0" $ do
        it "does not add Paths_" $ do
          [i|
          spec-version: 0.36.0
          library: {}
          |] `shouldRenderTo` library [i|
          default-language: Haskell2010
          |]

      context "when cabal-version is >= 2" $ do
        it "adds Paths_ to autogen-modules" $ do
          [i|
          verbatim:
            cabal-version: 2.0
          library: {}
          |] `shouldRenderTo` (library [i|
          other-modules:
              Paths_my_package
          autogen-modules:
              Paths_my_package
          default-language: Haskell2010
          |]) { packageCabalVersion = "2.0" }

        context "when Paths_ module is listed explicitly under generated-other-modules" $ do
          it "adds Paths_ to autogen-modules only once" $ do
            [i|
            verbatim:
              cabal-version: 2.0
            library:
              generated-other-modules: Paths_my_package
            |] `shouldRenderTo` (library [i|
            other-modules:
                Paths_my_package
            autogen-modules:
                Paths_my_package
            default-language: Haskell2010
            |]) { packageCabalVersion = "2.0" }

        context "when Paths_ module is listed explicitly under generated-exposed-modules" $ do
          it "adds Paths_ to autogen-modules only once" $ do
            [i|
            verbatim:
              cabal-version: 2.0
            library:
              generated-exposed-modules: Paths_my_package
            |] `shouldRenderTo` (library [i|
            exposed-modules:
                Paths_my_package
            autogen-modules:
                Paths_my_package
            default-language: Haskell2010
            |]) { packageCabalVersion = "2.0" }

      context "when Paths_ is mentioned in a conditional that is always false" $ do
        it "does not add Paths_" $ do
          [i|
          library:
            when:
            - condition: false
              other-modules: Paths_my_package
          |] `shouldRenderTo` library [i|
          default-language: Haskell2010
          |]

      context "when Paths_ is used with RebindableSyntax and (OverloadedStrings or OverloadedLists)" $ do
        it "infers cabal-version 2.2" $ do
          [i|
          default-extensions: [RebindableSyntax, OverloadedStrings]
          library: {}
          |] `shouldRenderTo` (library [i|
          default-extensions:
              RebindableSyntax
              OverloadedStrings
          other-modules:
              Paths_my_package
          autogen-modules:
              Paths_my_package
          default-language: Haskell2010
          |]) {packageCabalVersion = "2.2"}

        context "when Paths_ is mentioned in a conditional that is always false" $ do
          it "does not infer cabal-version 2.2" $ do
            [i|
            default-extensions: [RebindableSyntax, OverloadedStrings]
            library:
              when:
              - condition: false
                other-modules: Paths_my_package
            |] `shouldRenderTo` (library [i|
            default-extensions:
                RebindableSyntax
                OverloadedStrings
            default-language: Haskell2010
            |])

    describe "spec-version" $ do
      it "accepts spec-version" $ do
        [i|
        spec-version: 0.29.5
        |] `shouldRenderTo` package [i|
        |]

      it "fails on malformed spec-version" $ do
        [i|
        spec-version: foo
        |] `shouldFailWith` "package.yaml: Error while parsing $.spec-version - invalid value \"foo\""

      it "fails on unsupported spec-version" $ do
        [i|
        spec-version: 25.0
        |] `shouldFailWith` ("The file package.yaml requires version 25.0 of the Hpack package specification, however this version of hpack only supports versions up to " ++ showVersion Hpack.version ++ ". Upgrading to the latest version of hpack may resolve this issue.")

      it "fails on unsupported spec-version from defaults" $ do
        let file = joinPath ["defaults", "sol", "hpack-template", "2017", "defaults.yaml"]
        writeFile file [i|
        spec-version: 25.0
        |]

        [i|
        defaults:
          github: sol/hpack-template
          path: defaults.yaml
          ref: "2017"
        library: {}
        |] `shouldFailWith` ("The file " ++ file ++ " requires version 25.0 of the Hpack package specification, however this version of hpack only supports versions up to " ++ showVersion Hpack.version ++ ". Upgrading to the latest version of hpack may resolve this issue.")

    describe "data-files" $ do
      it "accepts data-files" $ do
        touch "data/foo/index.html"
        touch "data/bar/index.html"
        [i|
        data-files:
          - data/**/*.html
        |] `shouldRenderTo` package [i|
        data-files:
            data/bar/index.html
            data/foo/index.html
        |]

    describe "data-dir" $ do
      it "accepts data-dir" $ do
        touch "data/foo.html"
        touch "data/bar.html"
        [i|
        data-dir: data
        data-files:
          - "*.html"
        |] `shouldRenderTo` package [i|
        data-files:
            bar.html
            foo.html
        data-dir: data
        |]

    describe "github" $ do
      it "accepts owner/repo" $ do
        [i|
        github: sol/hpack
        |] `shouldRenderTo` package [i|
        homepage: https://github.com/sol/hpack#readme
        bug-reports: https://github.com/sol/hpack/issues
        source-repository head
          type: git
          location: https://github.com/sol/hpack
        |]

      it "accepts owner/repo/path" $ do
        [i|
        github: hspec/hspec/hspec-core
        |] `shouldRenderTo` package [i|
        homepage: https://github.com/hspec/hspec#readme
        bug-reports: https://github.com/hspec/hspec/issues
        source-repository head
          type: git
          location: https://github.com/hspec/hspec
          subdir: hspec-core
        |]

      it "rejects URLs" $ do
        [i|
        github: https://github.com/sol/hpack/issues/365
        |] `shouldFailWith` "package.yaml: Error while parsing $.github - expected owner/repo or owner/repo/subdir, but encountered \"https://github.com/sol/hpack/issues/365\""

    describe "homepage" $ do
      it "accepts homepage URL" $ do
        [i|
        homepage: https://example.com/
        |] `shouldRenderTo` package [i|
        homepage: https://example.com/
        |]

      context "with github" $ do
        it "gives homepage URL precedence" $ do
          [i|
          github: hspec/hspec
          homepage: https://example.com/
          |] `shouldRenderTo` package [i|
          homepage: https://example.com/
          bug-reports: https://github.com/hspec/hspec/issues
          source-repository head
            type: git
            location: https://github.com/hspec/hspec
          |]

        it "omits homepage URL if it is null" $ do
          [i|
          github: hspec/hspec
          homepage: null
          |] `shouldRenderTo` package [i|
          bug-reports: https://github.com/hspec/hspec/issues
          source-repository head
            type: git
            location: https://github.com/hspec/hspec
          |]

    describe "bug-reports" $ do
      it "accepts bug-reports URL" $ do
        [i|
        bug-reports: https://example.com/
        |] `shouldRenderTo` package [i|
        bug-reports: https://example.com/
        |]

      context "with github" $ do
        it "gives bug-reports URL precedence" $ do
          [i|
          github: hspec/hspec
          bug-reports: https://example.com/
          |] `shouldRenderTo` package [i|
          homepage: https://github.com/hspec/hspec#readme
          bug-reports: https://example.com/
          source-repository head
            type: git
            location: https://github.com/hspec/hspec
          |]

        it "omits bug-reports URL if it is null" $ do
          [i|
          github: hspec/hspec
          bug-reports: null
          |] `shouldRenderTo` package [i|
          homepage: https://github.com/hspec/hspec#readme
          source-repository head
            type: git
            location: https://github.com/hspec/hspec
          |]

    describe "defaults" $ do
      it "accepts global defaults" $ do
        writeFile "defaults/sol/hpack-template/2017/defaults.yaml" [i|
        default-extensions:
          - RecordWildCards
          - DeriveFunctor
        |]

        [i|
        defaults:
          github: sol/hpack-template
          path: defaults.yaml
          ref: "2017"
        library: {}
        |] `shouldRenderTo` library_ [i|
        default-extensions:
            RecordWildCards
            DeriveFunctor
        |]

      it "accepts library defaults" $ do
        writeFile "defaults/sol/hpack-template/2017/defaults.yaml" [i|
        exposed-modules: Foo
        |]

        [i|
        library:
          defaults:
            github: sol/hpack-template
            path: defaults.yaml
            ref: "2017"
        |] `shouldRenderTo` library_ [i|
        exposed-modules:
            Foo
        |]

      it "accepts executable defaults" $ do
        writeFile "defaults/sol/hpack-template/2017/.hpack/defaults.yaml" [i|
        main: Foo.hs
        |]

        [i|
        executable:
          defaults: sol/hpack-template@2017
        |] `shouldRenderTo` executable_ "my-package" [i|
        main-is: Foo.hs
        |]

      it "gives `main` from executable section precedence" $ do
        writeFile "defaults/sol/hpack-template/2017/.hpack/defaults.yaml" [i|
        main: Foo.hs
        |]

        [i|
        executable:
          main: Bar.hs
          defaults: sol/hpack-template@2017
        |] `shouldRenderTo` executable_ "my-package" [i|
        main-is: Bar.hs
        |]

      it "accepts a list of defaults" $ do
        writeFile "defaults/foo/bar/v1/.hpack/defaults.yaml" "default-extensions: RecordWildCards"
        writeFile "defaults/foo/bar/v2/.hpack/defaults.yaml" "default-extensions: DeriveFunctor"
        [i|
        defaults:
          - foo/bar@v1
          - foo/bar@v2
        library: {}
        |] `shouldRenderTo` library_ [i|
        default-extensions:
            RecordWildCards
            DeriveFunctor
        |]

      it "accepts defaults recursively" $ do
        writeFile "defaults/foo/bar/v1/.hpack/defaults.yaml" "defaults: foo/bar@v2"
        writeFile "defaults/foo/bar/v2/.hpack/defaults.yaml" "default-extensions: DeriveFunctor"
        [i|
        defaults: foo/bar@v1
        library: {}
        |] `shouldRenderTo` library_ [i|
        default-extensions:
            DeriveFunctor
        |]

      it "fails on cyclic defaults" $ do
        let
          file1 = "defaults/foo/bar/v1/.hpack/defaults.yaml"
          file2 = "defaults/foo/bar/v2/.hpack/defaults.yaml"
        writeFile file1 "defaults: foo/bar@v2"
        writeFile file2 "defaults: foo/bar@v1"
        canonic1 <- canonicalizePath file1
        canonic2 <- canonicalizePath file2
        [i|
        defaults: foo/bar@v1
        library: {}
        |] `shouldFailWith` [i|cycle in defaults (#{canonic1} -> #{canonic2} -> #{canonic1})|]

      it "fails if defaults don't exist" $ do
        pending
        [i|
        defaults:
          github: sol/foo
          ref: bar
        library: {}
        |] `shouldFailWith` "Invalid value for \"defaults\"! File https://raw.githubusercontent.com/sol/foo/bar/.hpack/defaults.yaml does not exist!"

      it "fails on parse error" $ do
        let file = joinPath ["defaults", "sol", "hpack-template", "2017", "defaults.yaml"]
        writeFile file "[]"
        [i|
        defaults:
          github: sol/hpack-template
          path: defaults.yaml
          ref: "2017"
        library: {}
        |] `shouldFailWith` (file ++ ": Error while parsing $ - expected Object, but encountered Array")

      it "warns on unknown fields" $ do
        let file = joinPath ["defaults", "sol", "hpack-template", "2017", "defaults.yaml"]
        writeFile file "foo: bar"
        [i|
        name: foo
        defaults:
          github: sol/hpack-template
          path: defaults.yaml
          ref: "2017"
          bar: baz
        library: {}
        |] `shouldWarn` [
            "package.yaml: Ignoring unrecognized field $.defaults.bar"
          , file ++ ": Ignoring unrecognized field $.foo"
          ]

      it "accepts defaults from local files" $ do
        writeFile "defaults/foo.yaml" [i|
        defaults:
          local: bar.yaml
        |]

        writeFile "defaults/bar.yaml" [i|
        default-extensions:
          - RecordWildCards
          - DeriveFunctor
        |]

        [i|
        defaults:
          local: defaults/foo.yaml
        library: {}
        |] `shouldRenderTo` library_ [i|
        default-extensions:
            RecordWildCards
            DeriveFunctor
        |]

    describe "version" $ do
      it "accepts string" $ do
        [i|
        version: 0.1.0
        |] `shouldRenderTo` (package "") {packageVersion = "0.1.0"}

      it "accepts number" $ do
        [i|
        version: 0.1
        |] `shouldRenderTo` (package [i|
        |]) {packageVersion = "0.1"}

      it "rejects other values" $ do
        [i|
        version: {}
        |] `shouldFailWith` "package.yaml: Error while parsing $.version - expected Number or String, but encountered Object"

    describe "license" $ do
      it "accepts cabal-style licenses" $ do
        [i|
        license: BSD3
        |] `shouldRenderTo` (package [i|
        license: BSD3
        |])

      it "accepts SPDX licenses" $ do
        [i|
        license: BSD-3-Clause
        |] `shouldRenderTo` (package [i|
        license: BSD-3-Clause
        |]) {packageCabalVersion = "2.2"}

      context "with an ambiguous license" $ do
        it "treats it as a cabal-style license" $ do
          [i|
          license: MIT
          |] `shouldRenderTo` (package [i|
          license: MIT
          |])

      context "when cabal-version >= 2.2" $ do
        it "maps license to SPDX license identifier" $ do
          [i|
          license: BSD3
          library:
            cxx-options: -Wall
          |] `shouldRenderTo` (package [i|
          license: BSD-3-Clause
          library
            other-modules:
                Paths_my_package
            autogen-modules:
                Paths_my_package
            cxx-options: -Wall
            default-language: Haskell2010
          |]) {packageCabalVersion = "2.2"}

        it "doesn't touch unknown licenses" $ do
          [i|
          license: some-license
          library:
            cxx-options: -Wall
          |] `shouldRenderTo` (package [i|
          license: some-license
          library
            other-modules:
                Paths_my_package
            autogen-modules:
                Paths_my_package
            cxx-options: -Wall
            default-language: Haskell2010
          |]) {packageCabalVersion = "2.2"}

      context "with a LICENSE file" $ do
        before_ (writeFile "LICENSE" license) $ do
          it "infers license" $ do
            [i|
            |] `shouldRenderTo` (package [i|
            license-file: LICENSE
            license: MIT
            |])

          context "when license can not be inferred" $ do
            it "warns" $ do
              writeFile "LICENSE" "some-licenese"
              [i|
              name: foo
              |] `shouldWarn` ["Inferring license from file LICENSE failed!"]

          context "when license is null" $ do
            it "does not infer license" $ do
              [i|
              license: null
              |] `shouldRenderTo` (package [i|
              license-file: LICENSE
              |])

    describe "build-type" $ do
      it "accept Simple" $ do
        [i|
        build-type: Simple
        |] `shouldRenderTo` (package "") {packageBuildType = "Simple"}

      it "accept Configure" $ do
        [i|
        build-type: Configure
        |] `shouldRenderTo` (package "") {packageBuildType = "Configure"}

      it "accept Make" $ do
        [i|
        build-type: Make
        |] `shouldRenderTo` (package "") {packageBuildType = "Make"}

      it "accept Custom" $ do
        [i|
        build-type: Custom
        |] `shouldRenderTo` (package "") {packageBuildType = "Custom"}

      it "rejects invalid values" $ do
        [i|
        build-type: foo
        |] `shouldFailWith` "package.yaml: Error while parsing $.build-type - expected one of Simple, Configure, Make, or Custom"


    describe "extra-doc-files" $ do
      it "accepts a list of files" $ do
        touch "CHANGES.markdown"
        touch "README.markdown"
        [i|
        extra-doc-files:
          - CHANGES.markdown
          - README.markdown
        |] `shouldRenderTo` (package [i|
        extra-doc-files:
            CHANGES.markdown
            README.markdown
        |]) {packageCabalVersion = "1.18"}

      it "accepts glob patterns" $ do
        touch "CHANGES.markdown"
        touch "README.markdown"
        [i|
        extra-doc-files:
          - "*.markdown"
        |] `shouldRenderTo` (package [i|
        extra-doc-files:
            CHANGES.markdown
            README.markdown
        |]) {packageCabalVersion = "1.18"}

      it "warns if a glob pattern does not match anything" $ do
        [i|
        name: foo
        extra-doc-files:
          - "*.markdown"
        |] `shouldWarn` ["Specified pattern \"*.markdown\" for extra-doc-files does not match any files"]

    describe "extra-files" $ do
      it "accepts extra-files" $ do
        touch "CHANGES.markdown"
        touch "README.markdown"
        [i|
        extra-files:
          - CHANGES.markdown
          - README.markdown
        |] `shouldRenderTo` (package [i|
        extra-files:
            CHANGES.markdown
            README.markdown
        |]) {packageCabalVersion = "3.14"}

    describe "build-tools" $ do
      context "with known build tools" $ do
        context "when cabal-version < 2" $ do
          it "adds them to build-tools" $ do
            [i|
            executable:
              build-tools:
                alex == 0.1.0
            |] `shouldRenderTo` executable_ "my-package" [i|
            build-tools:
                alex ==0.1.0
            |]

        context "when cabal-version >= 2" $ do
          it "adds them to build-tool-depends" $ do
            [i|
            verbatim:
              cabal-version: 2.0
            executable:
              build-tools:
                alex == 0.1.0
            |] `shouldRenderTo` (executable_ "my-package" [i|
            autogen-modules:
                Paths_my_package
            build-tool-depends:
                alex:alex ==0.1.0
            |]) {
              packageCabalVersion = "2.0"
            }

      context "with other build tools" $ do
        it "adds them to build-tool-depends" $ do
          [i|
          executable:
            build-tools:
              hspec-discover: 0.1.0
          |] `shouldRenderTo` (executable_ "my-package" [i|
          build-tool-depends:
              hspec-discover:hspec-discover ==0.1.0
          |]) {
            -- NOTE: We do not set this to 2.0 on purpose, so that the .cabal
            -- file is compatible with a wider range of Cabal versions!
            packageCabalVersion = "1.12"
          }

        it "accepts build-tool-depends as an alias" $ do
          [i|
          executable:
            build-tool-depends:
              hspec-discover: 0.1.0
          |] `shouldRenderTo` (executable_ "my-package" [i|
          build-tool-depends:
              hspec-discover:hspec-discover ==0.1.0
          |]) {
            packageCabalVersion = "1.12"
          , packageWarnings = ["package.yaml: $.executable.build-tool-depends is deprecated, use $.executable.build-tools instead"]
          }

      context "when the name of a build tool matches an executable from the same package" $ do
        it "adds it to build-tools" $ do
          [i|
          executables:
            bar:
              build-tools:
                - bar
          |] `shouldRenderTo` executable_ "bar" [i|
          build-tools:
              bar
          |]

        it "gives per-section unqualified names precedence over global qualified names" $ do
          [i|
          build-tools:
            - my-package:bar == 0.1.0
          executables:
            bar:
              build-tools:
                - bar == 0.2.0
          |] `shouldRenderTo` executable_ "bar" [i|
          build-tools:
              bar ==0.2.0
          |]

        it "gives per-section qualified names precedence over global unqualified names" $ do
          [i|
          build-tools:
            - bar == 0.1.0
          executables:
            bar:
              build-tools:
                - my-package:bar == 0.2.0
          |] `shouldRenderTo` executable_ "bar" [i|
          build-tools:
              bar ==0.2.0
          |]

        context "when cabal-version >= 2" $ do
          it "adds it to build-tool-depends" $ do
            [i|
            verbatim:
              cabal-version: 2.0
            executables:
              bar:
                build-tools:
                  - bar
            |] `shouldRenderTo` (executable_ "bar" [i|
            autogen-modules:
                Paths_my_package
            build-tool-depends:
                my-package:bar
            |]) {packageCabalVersion = "2.0"}

      context "when the name of a build tool matches a legacy system build tool" $ do
        it "adds it to build-tools" $ do
          [i|
          executable:
            build-tools:
              ghc >= 7.10
          |] `shouldRenderTo` (executable_ "my-package" [i|
          build-tools:
              ghc >=7.10
          |]) { packageWarnings = ["Listing \"ghc\" under build-tools is deperecated! Please list system executables under system-build-tools instead!"] }

    describe "system-build-tools" $ do
      it "adds system build tools to build-tools" $ do
        [i|
        executable:
          system-build-tools:
            ghc >= 7.10
        |] `shouldRenderTo` executable_ "my-package" [i|
        build-tools:
            ghc >=7.10
        |]

      context "with hpc" $ do
        it "infers cabal-version 1.14" $ do
          [i|
          executable:
            system-build-tools:
              hpc
          |] `shouldRenderTo` (executable_ "my-package" [i|
          build-tools:
              hpc
          |]) {packageCabalVersion = "1.14"}

      context "with ghcjs" $ do
        it "infers cabal-version 1.22" $ do
          [i|
          executable:
            system-build-tools:
              ghcjs
          |] `shouldRenderTo` (executable_ "my-package" [i|
          build-tools:
              ghcjs
          |]) {packageCabalVersion = "1.22"}

      context "with an unknown system build tool" $ do
        it "infers cabal-version 2.0" $ do
          [i|
          executable:
            system-build-tools:
              g++ >= 5.4.0
          |] `shouldRenderTo` (executable_ "my-package" [i|
          autogen-modules:
              Paths_my_package
          build-tools:
              g++ >=5.4.0
          |]) {packageCabalVersion = "2.0"}

    describe "dependencies" $ do
      it "accepts single dependency" $ do
        [i|
        executable:
          dependencies: base
        |] `shouldRenderTo` executable_ "my-package" [i|
        build-depends:
            base
        |]

      it "accepts build-depends as an alias" $ do
        [i|
        executable:
          build-depends: base
        |] `shouldRenderTo` (executable_ "my-package" [i|
        build-depends:
            base
        |]) {
          packageWarnings = ["package.yaml: $.executable.build-depends is deprecated, use $.executable.dependencies instead"]
        }

      it "accepts dependencies with subcomponents" $ do
        [i|
        executable:
          dependencies: foo:bar
        |] `shouldRenderTo` (executable_ "my-package" [i|
        autogen-modules:
            Paths_my_package
        build-depends:
            foo:bar
        |]) {packageCabalVersion = "3.0"}

      it "accepts list of dependencies" $ do
        [i|
        executable:
          dependencies:
            - base
            - transformers
        |] `shouldRenderTo` executable_ "my-package" [i|
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
          |] `shouldRenderTo` executable_ "my-package" [i|
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
          |] `shouldRenderTo` executable_ "my-package" [i|
          build-depends:
              base >=2
          |]

    describe "pkg-config-dependencies" $ do
      it "accepts pkg-config-dependencies" $ do
        [i|
        pkg-config-dependencies:
          - QtWebKit
          - weston
        executable: {}
        |] `shouldRenderTo` executable_ "my-package" [i|
        pkgconfig-depends:
            QtWebKit
          , weston
        |]

      it "accepts pkgconfig-depends as an alias" $ do
        [i|
        pkgconfig-depends:
          - QtWebKit
          - weston
        executable: {}
        |] `shouldRenderTo` executable_ "my-package" [i|
        pkgconfig-depends:
            QtWebKit
          , weston
        |]

    describe "include-dirs" $ do
      it "accepts include-dirs" $ do
        [i|
        include-dirs:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable_ "my-package" [i|
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
        |] `shouldRenderTo` executable_ "my-package" [i|
        install-includes:
            foo.h
            bar.h
        |]

    describe "ghc-shared-options" $ do
      it "accepts ghc-shared-options" $ do
        [i|
        ghc-shared-options: -Wall
        executable: {}
        |] `shouldRenderTo` executable_ "my-package" [i|
        ghc-shared-options: -Wall
        |]

    describe "js-sources" $ before_ (touch "foo.js" >> touch "jsbits/bar.js") $ do
      it "accepts js-sources" $ do
        [i|
        executable:
          js-sources:
            - foo.js
            - jsbits/*.js
        |] `shouldRenderTo` executable_ "my-package" [i|
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
        |] `shouldRenderTo` executable_ "my-package" [i|
        js-sources:
            foo.js
            jsbits/bar.js
        |]

    describe "asm-options" $ do
      it "accepts asm-options" $ do
        [i|
        executable:
          asm-options: -Wall
        |] `shouldRenderTo` (executable_ "my-package" [i|
        autogen-modules:
            Paths_my_package
        asm-options: -Wall
        |]) {packageCabalVersion = "3.0"}

    describe "asm-sources" $ before_ (touch "foo.asm" >> touch "asmbits/bar.asm") $ do
      it "accepts asm-sources" $ do
        [i|
        executable:
          asm-sources:
            - foo.asm
            - asmbits/*.asm
        |] `shouldRenderTo` (executable_ "my-package" [i|
        autogen-modules:
            Paths_my_package
        asm-sources:
            foo.asm
            asmbits/bar.asm
        |]) {packageCabalVersion = "3.0"}

    describe "cxx-options" $ do
      it "accepts cxx-options" $ do
        [i|
        executable:
          cxx-options: -Wall
        |] `shouldRenderTo` (executable_ "my-package" [i|
        autogen-modules:
            Paths_my_package
        cxx-options: -Wall
        |]) {packageCabalVersion = "2.2"}

      context "when used inside a nested conditional" $ do
        it "infers correct cabal-version" $ do
          [i|
          executable:
            when:
              condition: True
              when:
                condition: True
                when:
                  condition: True
                  cxx-options: -Wall
          |] `shouldRenderTo` (executable "my-package" [i|
          other-modules:
              Paths_my_package
          autogen-modules:
              Paths_my_package
          default-language: Haskell2010
          if true
            if true
              if true
                cxx-options: -Wall
          |]) {packageCabalVersion = "2.2"}

    describe "cxx-sources" $ before_ (touch "foo.cc" >> touch "cxxbits/bar.cc") $ do
      it "accepts cxx-sources" $ do
        [i|
        executable:
          cxx-sources:
            - foo.cc
            - cxxbits/*.cc
        |] `shouldRenderTo` (executable_ "my-package" [i|
        autogen-modules:
            Paths_my_package
        cxx-sources:
            foo.cc
            cxxbits/bar.cc
        |]) {packageCabalVersion = "2.2"}

    describe "language" $ do
      it "accepts language" $ do
        [i|
        language: GHC2021
        executable: {}
        |] `shouldRenderTo` executable "my-package" [i|
          other-modules:
              Paths_my_package
          default-language: GHC2021
        |]

      it "omits language if it is null" $ do
        [i|
        language: null
        executable: {}
        |] `shouldRenderTo` executable "my-package" [i|
          other-modules:
              Paths_my_package
        |]

      it "accepts default-language as an alias" $ do
        [i|
        default-language: GHC2021
        executable: {}
        |] `shouldRenderTo` (executable "my-package" [i|
          other-modules:
              Paths_my_package
          default-language: GHC2021
        |]) {
          packageWarnings = ["package.yaml: $.default-language is deprecated, use $.language instead"]
        }

      it "gives section-level language precedence" $ do
        [i|
        language: Haskell2010
        executable:
          language: GHC2021
        |] `shouldRenderTo` executable "my-package" [i|
          other-modules:
              Paths_my_package
          default-language: GHC2021
        |]

      it "accepts language from defaults" $ do
        writeFile "defaults/sol/hpack-template/2017/.hpack/defaults.yaml" [i|
        language: GHC2021
        |]

        [i|
        defaults: sol/hpack-template@2017
        library: {}
        |] `shouldRenderTo` library [i|
        other-modules:
            Paths_my_package
        default-language: GHC2021
        |]

    describe "extra-lib-dirs" $ do
      it "accepts extra-lib-dirs" $ do
        [i|
        extra-lib-dirs:
          - foo
          - bar
        executable: {}
        |] `shouldRenderTo` executable_ "my-package" [i|
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
        |] `shouldRenderTo` executable_ "my-package" [i|
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
        |] `shouldRenderTo` executable_ "my-package" [i|
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
        |] `shouldRenderTo` executable_ "my-package" [i|
        frameworks:
            foo
            bar
        |]

    describe "c-sources" $ before_ (touch "cbits/foo.c" >> touch "cbits/bar.c" >> touch "cbits/baz.c") $ do
      it "keeps declaration order" $ do
        -- IMPORTANT: This is crucial as a workaround for https://ghc.haskell.org/trac/ghc/ticket/13786
        [i|
        library:
          c-sources:
            - cbits/foo.c
            - cbits/bar.c
            - cbits/baz.c
        |] `shouldRenderTo` library_ [i|
        c-sources:
            cbits/foo.c
            cbits/bar.c
            cbits/baz.c
        |]

      it "accepts glob patterns" $ do
        [i|
        library:
          c-sources: cbits/*.c
        |] `shouldRenderTo` library_ [i|
        c-sources:
            cbits/bar.c
            cbits/baz.c
            cbits/foo.c
        |]

      it "warns when a glob pattern does not match any files" $ do
        [i|
        name: foo
        library:
          c-sources: foo/*.c
        |] `shouldWarn` pure "Specified pattern \"foo/*.c\" for c-sources does not match any files"

      it "quotes filenames with special characters" $ do
        touch "cbits/foo bar.c"
        [i|
        library:
          c-sources:
            - cbits/foo bar.c
        |] `shouldRenderTo` library_ [i|
        c-sources:
            "cbits/foo bar.c"
        |]

    describe "custom-setup" $ do
      it "warns on unknown fields" $ do
        [i|
        name: foo
        custom-setup:
          foo: 1
          bar: 2
        |] `shouldWarn` [
            "package.yaml: Ignoring unrecognized field $.custom-setup.bar"
          , "package.yaml: Ignoring unrecognized field $.custom-setup.foo"
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

    describe "library" $ do
      it "accepts reexported-modules" $ do
        [i|
        library:
          reexported-modules: Baz
        |] `shouldRenderTo` (library_ [i|
          reexported-modules:
              Baz
        |]) {packageCabalVersion = "1.22"}

      it "accepts signatures" $ do
        [i|
        library:
          signatures: Foo
        |] `shouldRenderTo` (library_ [i|
          autogen-modules:
              Paths_my_package
          signatures:
              Foo
        |]) {packageCabalVersion = "2.0"}

      context "when package.yaml contains duplicate modules" $ do
        it "generates a cabal file with duplicate modules" $ do
          -- garbage in, garbage out
          [i|
          library:
            exposed-modules: Foo
            other-modules: Foo
          |] `shouldRenderTo` library [i|
          exposed-modules:
              Foo
          other-modules:
              Foo
          default-language: Haskell2010
          |]

      context "with mixins" $ do
        it "infers cabal-version 2.0" $ do
          [i|
          library:
            dependencies:
              foo:
                mixin:
                  - (Blah as Etc)
          |] `shouldRenderTo` (library [i|
          other-modules:
              Paths_my_package
          autogen-modules:
              Paths_my_package
          build-depends:
              foo
          mixins:
              foo (Blah as Etc)
          default-language: Haskell2010
          |]) {packageCabalVersion = "2.0"}

    describe "internal-libraries" $ do
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
            Paths_my_package
        autogen-modules:
            Paths_my_package
        hs-source-dirs:
            src
        |]

      it "warns on unknown fields" $ do
        [i|
        name: foo
        internal-libraries:
          bar:
            baz: 42
        |] `shouldWarn` pure "package.yaml: Ignoring unrecognized field $.internal-libraries.bar.baz"

      it "warns on missing source-dirs" $ do
        [i|
        name: foo
        internal-libraries:
          bar:
            source-dirs: src
        |] `shouldWarn` pure "Specified source-dir \"src\" does not exist"

      it "accepts visibility" $ do
        [i|
        internal-libraries:
          bar:
            visibility: public
        |] `shouldRenderTo` (internalLibrary "bar" [i|
        visibility: public
        other-modules:
            Paths_my_package
        autogen-modules:
            Paths_my_package
        |]) {packageCabalVersion = "3.0"}

    context "when inferring modules" $ do
      context "with a library" $ do
        it "ignores duplicate source directories" $ do
          touch "src/Foo.hs"
          [i|
          source-dirs: src
          library:
            source-dirs: src
          |] `shouldRenderTo` library [i|
          hs-source-dirs:
              src
          exposed-modules:
              Foo
          other-modules:
              Paths_my_package
          default-language: Haskell2010
          |]

        it "ignores duplicate modules" $ do
          touch "src/Foo.hs"
          touch "src/Foo.x"
          [i|
          library:
            source-dirs: src
          |] `shouldRenderTo` library [i|
          hs-source-dirs:
              src
          exposed-modules:
              Foo
          other-modules:
              Paths_my_package
          default-language: Haskell2010
          |]

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
                Paths_my_package
            default-language: Haskell2010
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
            default-language: Haskell2010
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
            default-language: Haskell2010
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
                Paths_my_package
            default-language: Haskell2010
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
                exposed-modules:
                  - Foo
                  - Paths_my_package
            |] `shouldRenderTo` package [i|
            library
              hs-source-dirs:
                  src
              exposed-modules:
                  Bar
              default-language: Haskell2010
              if os(windows)
                exposed-modules:
                    Foo
                    Paths_my_package
            |]

          context "with a source-dir inside the conditional" $ do
            it "infers other-modules" $ do
              touch "windows/Foo.hs"
              [i|
              library:
                when:
                  condition: os(windows)
                  source-dirs: windows
              |] `shouldRenderTo` package [i|
              library
                other-modules:
                    Paths_my_package
                default-language: Haskell2010
                if os(windows)
                  other-modules:
                      Foo
                  hs-source-dirs:
                      windows
              |]

            it "does not infer outer modules" $ do
              touch "windows/Foo.hs"
              touch "unix/Foo.hs"
              [i|
              library:
                exposed-modules: Foo
                when:
                  condition: os(windows)
                  then:
                    source-dirs: windows/
                  else:
                    source-dirs: unix/

              |] `shouldRenderTo` package [i|
              library
                exposed-modules:
                    Foo
                other-modules:
                    Paths_my_package
                default-language: Haskell2010
                if os(windows)
                  hs-source-dirs:
                      windows/
                else
                  hs-source-dirs:
                      unix/
              |]

        context "with generated modules" $ do
          it "includes generated modules in autogen-modules" $ do
            [i|
            library:
              generated-exposed-modules: Foo
              generated-other-modules: Bar
            |] `shouldRenderTo` (library [i|
            exposed-modules:
                Foo
            other-modules:
                Paths_my_package
                Bar
            autogen-modules:
                Paths_my_package
                Foo
                Bar
            default-language: Haskell2010
            |]) {packageCabalVersion = "2.0"}

          it "does not infer any mentioned generated modules" $ do
            touch "src/Exposed.hs"
            touch "src/Other.hs"
            [i|
            library:
              source-dirs: src
              generated-exposed-modules: Exposed
              generated-other-modules: Other
            |] `shouldRenderTo` (library [i|
            hs-source-dirs:
                src
            exposed-modules:
                Exposed
            other-modules:
                Paths_my_package
                Other
            autogen-modules:
                Paths_my_package
                Exposed
                Other
            default-language: Haskell2010
            |]) {packageCabalVersion = "2.0"}

          it "does not infer any generated modules mentioned inside conditionals" $ do
            touch "src/Exposed.hs"
            touch "src/Other.hs"
            [i|
            library:
              source-dirs: src
              when:
                condition: os(windows)
                generated-exposed-modules: Exposed
                generated-other-modules: Other
            |] `shouldRenderTo` (package [i|
            library
              other-modules:
                  Paths_my_package
              autogen-modules:
                  Paths_my_package
              hs-source-dirs:
                  src
              default-language: Haskell2010
              if os(windows)
                exposed-modules:
                    Exposed
                other-modules:
                    Other
                autogen-modules:
                    Other
                    Exposed
            |]) {packageCabalVersion = "2.0"}

      context "with an executable" $ do
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
                Paths_my_package
            default-language: Haskell2010
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
            default-language: Haskell2010
          |]

        it "does not infer any mentioned generated modules" $ do
          touch "src/Foo.hs"
          [i|
          executables:
            foo:
              main: Main.hs
              source-dirs: src
              generated-other-modules: Foo
          |] `shouldRenderTo` (executable "foo" [i|
            main-is: Main.hs
            hs-source-dirs:
                src
            other-modules:
                Paths_my_package
                Foo
            autogen-modules:
                Paths_my_package
                Foo
            default-language: Haskell2010
          |]) {packageCabalVersion = "2.0"}

        context "with a conditional" $ do
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
                Paths_my_package
            hs-source-dirs:
                src
            default-language: Haskell2010
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
                Paths_my_package
            hs-source-dirs:
                src
            default-language: Haskell2010
            if os(windows)
              other-modules:
                  Bar
              hs-source-dirs:
                  windows
            |]

    describe "executables" $ do
      it "accepts main-is as an alias for main" $ do
        [i|
        executable:
          main-is: Foo.hs
        |] `shouldRenderTo` (executable_ "my-package" [i|
        main-is: Foo.hs
        |]) {
          packageWarnings = ["package.yaml: $.executable.main-is is deprecated, use $.executable.main instead"]
        }

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
            Paths_my_package
        default-language: Haskell2010
        |]

      context "with a conditional" $ do
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
          other-modules:
              Paths_my_package
          default-language: Haskell2010
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
          other-modules:
              Paths_my_package
          default-language: Haskell2010
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
        |] `shouldRenderTo` executable "my-package" [i|
        other-modules:
            Paths_my_package
        default-language: Haskell2010
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
            "package.yaml: Ignoring unrecognized field $.foo"
          , "package.yaml: Ignoring unrecognized field $.when[0].bar"
          , "package.yaml: Ignoring unrecognized field $.when[0].when.bar2"
          , "package.yaml: Ignoring unrecognized field $.when[1].baz"
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
          |] `shouldRenderTo` executable "my-package" [i|
          other-modules:
              Paths_my_package
          default-language: Haskell2010
          if os(windows)
            build-depends:
                Win32
          else
            build-depends:
                unix
          |]

        context "with empty then-branch" $ do
          it "provides a hint" $ do
            [i|
            when:
              condition: os(windows)
              then: {}
              else:
                dependencies: unix
            executable: {}
            |] `shouldFailWith` unlines [
                "package.yaml: Error while parsing $.when - an empty \"then\" section is not allowed, try the following instead:"
              , ""
              , "when:"
              , "  condition: '!(os(windows))'"
              , "  dependencies: unix"
              ]

        context "with empty else-branch" $ do
          it "provides a hint" $ do
            [i|
            when:
              condition: os(windows)
              then:
                dependencies: Win32
              else: {}
            executable: {}
            |] `shouldFailWith` unlines [
                "package.yaml: Error while parsing $.when - an empty \"else\" section is not allowed, try the following instead:"
              , ""
              , "when:"
              , "  condition: os(windows)"
              , "  dependencies: Win32"
              ]

        it "rejects invalid conditionals" $ do
          [i|
          when:
            condition: os(windows)
            then:
              dependencies: Win32
            else: null
          |] `shouldFailWith` "package.yaml: Error while parsing $.when.else - expected Object, but encountered Null"

        it "rejects invalid conditionals" $ do
          [i|
            dependencies:
              - foo
              - 23
          |] `shouldFailWith` "package.yaml: Error while parsing $.dependencies[1] - expected Object or String, but encountered Number"

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
                then:
                  dependencies: foo
                else:
                  baz: null
          |] `shouldWarn` [
              "package.yaml: Ignoring unrecognized field $.when.foo"
            , "package.yaml: Ignoring unrecognized field $.when.then.bar"
            , "package.yaml: Ignoring unrecognized field $.when.else.when.else.baz"
            ]

    describe "verbatim" $ do
      it "accepts strings" $ do
        [i|
        library:
          verbatim: |
            foo: 23
            bar: 42
        |] `shouldRenderTo` package [i|
        library
          other-modules:
              Paths_my_package
          default-language: Haskell2010
          foo: 23
          bar: 42
        |]

      it "accepts multi-line strings as field values" $ do
        [i|
        library:
          verbatim:
            build-depneds: |
              foo
              bar
              baz
        |] `shouldRenderTo` package [i|
        library
          other-modules:
              Paths_my_package
          default-language: Haskell2010
          build-depneds:
              foo
              bar
              baz
        |]

      it "allows to null out existing fields" $ do
        [i|
        library:
          verbatim:
            default-language: null
        |] `shouldRenderTo` package [i|
        library
          other-modules:
              Paths_my_package
        |]

      context "when specified globally" $ do
        it "overrides header fields" $ do
          [i|
          verbatim:
            build-type: foo
          |] `shouldRenderTo` (package "") {packageBuildType = "foo"}

        context "with cabal-version" $ do
          context "with a string value" $ do
            it "takes precedence over inferred version" $ do
              [i|
              license: BSD-3-Clause
              verbatim:
                cabal-version: foo
              |] `shouldRenderTo` (package "license: BSD-3-Clause") {packageCabalVersion = "foo"}

          context "with a version" $ do
            it "takes precedence over inferred version" $ do
              [i|
              license: BSD-3-Clause
              verbatim:
                cabal-version: 0.8
              |] `shouldRenderTo` (package "license: BSD-3-Clause") {packageCabalVersion = "0.8"}

        it "overrides other fields" $ do
          touch "foo"
          [i|
          extra-source-files: foo
          verbatim:
            extra-source-files: bar
          |] `shouldRenderTo` package [i|
          extra-source-files: bar
          |]

        it "is not propagated into sections" $ do
          [i|
          verbatim:
            foo: 23
          library: {}
          |] `shouldRenderTo` package [i|
          foo: 23
          library
            other-modules:
                Paths_my_package
            default-language: Haskell2010
          |]

      context "within a section" $ do
        it "overrides section fields" $ do
          [i|
          tests:
            spec:
              verbatim:
                type: detailed-0.9
          |] `shouldRenderTo` package [i|
          test-suite spec
            type: detailed-0.9
            other-modules:
                Paths_my_package
            default-language: Haskell2010
          |]
    describe "default value of maintainer" $ do
      it "gives maintainer precedence" $ do
        [i|
          author: John Doe
          maintainer: Jane Doe
          |] `shouldRenderTo` package [i|
          author: John Doe
          maintainer: Jane Doe
          |]
      context "with author" $ do
        it "uses author if maintainer is not specified" $ do
          [i|
            author: John Doe
            |] `shouldRenderTo` package [i|
            author: John Doe
            maintainer: John Doe
            |]
        it "omits maintainer if it is null" $ do
          [i|
            author: John Doe
            maintainer: null
            |] `shouldRenderTo` package [i|
            author: John Doe
            |]

run :: HasCallStack => FilePath -> FilePath -> String -> IO ([String], String)
run userDataDir c old = run_ userDataDir c old >>= either assertFailure return

run_ :: FilePath -> FilePath -> String -> IO (Either String ([String], String))
run_ userDataDir c old = do
  mPackage <- readPackageConfig defaultDecodeOptions {decodeOptionsTarget = c, decodeOptionsUserDataDir = Just userDataDir}
  return $ case mPackage of
    Right (DecodeResult pkg cabalVersion _ warnings) ->
      let
        FormattingHints{..} = sniffFormattingHints (lines old)
        alignment = fromMaybe 0 formattingHintsAlignment
        settings = formattingHintsRenderSettings
        output = cabalVersion ++ Hpack.renderPackageWith settings alignment formattingHintsFieldOrder formattingHintsSectionsFieldOrder pkg
      in
        Right (warnings, output)
    Left err -> Left err

data RenderResult = RenderResult [String] String
  deriving Eq

instance Show RenderResult where
  show (RenderResult warnings output) = unlines (map ("WARNING: " ++) warnings) ++ output

shouldRenderTo :: HasCallStack => String -> Package -> Expectation
shouldRenderTo input p = do
  writeFile packageConfig ("name: my-package\n" ++ unindent input)
  (warnings, output) <- run "" packageConfig expected
  RenderResult warnings (dropEmptyLines output) `shouldBe` RenderResult (packageWarnings p) expected
  where
    expected = dropEmptyLines (renderPackage p)
    dropEmptyLines = unlines . filter (not . null) . lines

shouldWarn :: HasCallStack => String -> [String] -> Expectation
shouldWarn input expected = do
  writeFile packageConfig input
  (warnings, _) <- run "" packageConfig ""
  sort warnings `shouldBe` sort expected

shouldFailWith :: HasCallStack => String -> String -> Expectation
shouldFailWith input expected = do
  writeFile packageConfig input
  run_ "" packageConfig "" `shouldReturn` Left expected

customSetup :: String -> Package
customSetup a = (package content) {packageCabalVersion = "1.24", packageBuildType = "Custom"}
  where
    content = [i|
custom-setup
#{indentBy 2 $ unindent a}
|]

library_ :: String -> Package
library_ l = package content
  where
    content = [i|
library
  other-modules:
      Paths_my_package
#{indentBy 2 $ unindent l}
  default-language: Haskell2010
|]

library :: String -> Package
library l = package content
  where
    content = [i|
library
#{indentBy 2 $ unindent l}
|]

internalLibrary :: String -> String -> Package
internalLibrary name e = (package content) {packageCabalVersion = "2.0"}
  where
    content = [i|
library #{name}
#{indentBy 2 $ unindent e}
  default-language: Haskell2010
|]

executable_ :: String -> String -> Package
executable_ name e = package content
  where
    content = [i|
executable #{name}
  other-modules:
      Paths_my_package
#{indentBy 2 $ unindent e}
  default-language: Haskell2010
|]

executable :: String -> String -> Package
executable name e = package content
  where
    content = [i|
executable #{name}
#{indentBy 2 $ unindent e}
|]

package :: String -> Package
package c = Package "my-package" "0.0.0" "Simple" "1.12" c []

data Package = Package {
  packageName :: String
, packageVersion :: String
, packageBuildType :: String
, packageCabalVersion :: String
, packageContent :: String
, packageWarnings :: [String]
}

renderPackage :: Package -> String
renderPackage Package{..} = unindent [i|
cabal-version: #{packageCabalVersion}
name: #{packageName}
version: #{packageVersion}
build-type: #{packageBuildType}

#{unindent packageContent}
|]

indentBy :: Int -> String -> String
indentBy n = unlines . map (replicate n ' ' ++) . lines

license :: String
license = [i|
Copyright (c) 2014-2023 Simon Hengel <sol@typeful.net>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|]
