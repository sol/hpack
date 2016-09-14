{-# LANGUAGE QuasiQuotes #-}
module Hpack.OptionalExposureSpec where
import Data.String.Interpolate
import Data.Version (showVersion)
import Helper (withCurrentDirectory, withTempDirectory)
import Hpack (hpack, version)
import Hpack.OptionalExposure (BuildPlan(..))
import System.Directory (createDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Test.Mockery.Directory (touch)

withPackageConfig :: String -> IO () -> Expectation -> Expectation
withPackageConfig content beforeAction expectation = withTempDirectory $ \dir_ -> do
  let dir = dir_ </> "NoOrphans"
  createDirectory dir
  writeFile (dir </> "package.yaml") content
  withCurrentDirectory dir (beforeAction >> expectation)

-- | An Hspec package with an optionally exposed module (@Foo.Text@)
-- ostensibly to provide instances for a data type defined in the
-- @text@ package.
package :: String
package = [i|
name: NoOrphans
version: 0.1.0
synopsis: A demonstration module that provides instances only when needed dependencies are available.
maintainer: Anthony Cowley <acowley@gmail.com>
license: BSD3
category: Development
ghc-options: -Wall
dependencies:
  - base >= 4.7 && < 5
library:
  source-dirs: src
  exposed-modules:
    - Foo
  optionally-exposed:
    dependencies: text > 1.3
    modules: Foo.Text
|]

-- | A build plan that does not provide the @text@ package
buildPlanNoText :: String
buildPlanNoText = [i|NoOrphans 0.1.0
array 0.5.1.0
base 4.8.2.0
binary 0.7.5.0
bytestring 0.10.6.0
containers 0.5.6.2
deepseq 1.4.1.1
ghc-prim 0.4.0.0
integer-gmp 1.0.0.0
|]

-- | The expected Cabal file resulting from a build plan that does not
-- provide a suitable version of the @text@ package.
noTextCabal :: String
noTextCabal = [i|-- This file has been generated from package.yaml by hpack version #{showVersion version}.
--
-- see: https://github.com/sol/hpack

name:           NoOrphans
version:        0.1.0
synopsis:       A demonstration module that provides instances only when needed dependencies are available.
category:       Development
maintainer:     Anthony Cowley <acowley@gmail.com>
license:        BSD3
build-type:     Simple
cabal-version:  >= 1.10

library
  hs-source-dirs:
      src
  ghc-options: -Wall
  build-depends:
      base >= 4.7 && < 5
  exposed-modules:
      Foo
  other-modules:
      Paths_NoOrphans
  default-language: Haskell2010
|]

-- | A build plan with an old version of @text@
buildPlanOlderText :: String
buildPlanOlderText = [i|NoOrphans 0.1.0
array 0.5.1.0
base 4.8.2.0
binary 0.7.5.0
bytestring 0.10.6.0
containers 0.5.6.2
deepseq 1.4.1.1
ghc-prim 0.4.0.0
integer-gmp 1.0.0.0
text 1.2.2.1
|]

-- | A build plan with a new version of @text@ (new in that it
-- satisfies the constraint identified in 'package').
buildPlanNewerText :: String
buildPlanNewerText = [i|NoOrphans 0.1.0
array 0.5.1.0
base 4.8.2.0
binary 0.7.5.0
bytestring 0.10.6.0
containers 0.5.6.2
deepseq 1.4.1.1
ghc-prim 0.4.0.0
integer-gmp 1.0.0.0
text 1.4
|]

-- | The expected Cabal file resulting from a build plan that provides
-- a suitable version of the @text@ package.
newerTextCabal :: String
newerTextCabal = [i|-- This file has been generated from package.yaml by hpack version #{showVersion version}.
--
-- see: https://github.com/sol/hpack

name:           NoOrphans
version:        0.1.0
synopsis:       A demonstration module that provides instances only when needed dependencies are available.
category:       Development
maintainer:     Anthony Cowley <acowley@gmail.com>
license:        BSD3
build-type:     Simple
cabal-version:  >= 1.10

library
  hs-source-dirs:
      src
  ghc-options: -Wall
  build-depends:
      base >= 4.7 && < 5
    , text > 1.3
  exposed-modules:
      Foo
      Foo.Text
  other-modules:
      Paths_NoOrphans
  default-language: Haskell2010
|]

prepSourceTree :: IO ()
prepSourceTree =
  do touch "src/Foo.hs"
     touch "src/Foo/Text.hs"
     writeFile "buildPlanNoText" buildPlanNoText
     writeFile "buildPlanOlderText" buildPlanOlderText
     writeFile "buildPlanNewerText" buildPlanNewerText

spec :: Spec
spec = do it "Exports optional modules when no build plan is given" $ do
            withPackageConfig
              package
              (do prepSourceTree
                  hpack (Left MaximalBuildPlan) "." False)
              (readFile "NoOrphans.cabal" `shouldReturn` newerTextCabal)
          it "Hides when a minimal build plan is specified" $
            withPackageConfig
              package
              (do prepSourceTree
                  hpack (Left MinimalBuildPlan) "." False)
              (readFile "NoOrphans.cabal" `shouldReturn` noTextCabal)
          it "Exports modules when constraints are satisfied" $ do
            withPackageConfig
              package
              (do prepSourceTree
                  hpack (Right "buildPlanNewerText") "." False)
              (readFile "NoOrphans.cabal" `shouldReturn` newerTextCabal)
          it "Hides when constraints are not satisfied" $ do
            withPackageConfig
              package
              (do prepSourceTree
                  hpack (Right "buildPlanOlderText") "." False)
              (readFile "NoOrphans.cabal" `shouldReturn` noTextCabal)
          it "Hides when dependencies are not available" $ do
            withPackageConfig
              package
              (do prepSourceTree
                  hpack (Right "buildPlanNoText") "." False)
              (readFile "NoOrphans.cabal" `shouldReturn` noTextCabal)
