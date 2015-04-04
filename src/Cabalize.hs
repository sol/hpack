{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
module Cabalize (cabalize) where

import           Prelude ()
import           Prelude.Compat

import           Data.List (sort, intercalate)
import           Data.String.Interpolate
import           System.Exit.Compat

import           Util
import           Config

renderExecutables :: [Executable] -> String
renderExecutables = intercalate "\n" . map renderExecutable

renderExecutable :: Executable -> String
renderExecutable executable@Executable{..} = stripEmptyLines [i|
executable #{executableName}
|] ++ renderExecutableSection executable

renderTests :: [Executable] -> String
renderTests = intercalate "\n" . map renderTest

renderTest :: Executable -> String
renderTest executable@Executable{..} = stripEmptyLines [i|
test-suite #{executableName}
  type: exitcode-stdio-1.0
|] ++ renderExecutableSection executable


renderExecutableSection :: Executable -> String
renderExecutableSection Executable{..} = stripEmptyLines [i|
#{if null executableSourceDirs then "" else "  hs-source-dirs: " ++ intercalate ", " executableSourceDirs}
  main-is: #{executableMain}
  build-depends:
      #{intercalate "\n    , " $ sort executableDependencies}
  ghc-options: #{unwords executableGhcOptions}
  default-language: Haskell2010
|]

configFile :: FilePath
configFile = "package.yaml"

cabalize :: IO (FilePath, String)
cabalize = do
  mPackage <- readConfig configFile
  case mPackage of
    Just package -> do
      return (packageName package ++ ".cabal", renderPackage package)
    Nothing -> die [i|could not parse #{configFile}|]

renderPackage :: Package -> String
renderPackage Package{..} = stripEmptyLines [i|
-- This file has been generated from #{configFile} by Cabalize.
name: #{packageName}
version: #{packageVersion}#{maybe "" ("\nlicense: " ++) packageLicense}#{maybe "" ("\nlicense-file: " ++) packageLicenseFile}
build-type: Simple
cabal-version: >= 1.10

#{maybe "" renderLibrary packageLibrary}
#{renderExecutables packageExecutables}
#{renderTests packageTests}
|]

renderLibrary :: Library -> String
renderLibrary Library{..} = stripEmptyLines [i|
library
  hs-source-dirs: src
  exposed-modules:
#{intercalate "\n" . map ("      " ++) $ libraryExposedModules}
  other-modules:
#{intercalate "\n" . map ("      " ++) $ libraryOtherModules}
  build-depends:
      #{intercalate "\n    , " $ sort libraryDependencies}
  default-language: Haskell2010
|]

