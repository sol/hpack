{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
module Cabalize where

import           Prelude ()
import           Prelude.Compat

import           Data.List
import           Data.String.Interpolate
import           System.FilePath
import           System.Exit.Compat

import           Util
import           Config

renderTests :: [Test] -> String
renderTests = unlines . map renderTest

renderTest :: Test -> String
renderTest Test{..} = stripEmptyLines [i|
test-suite spec
  type: exitcode-stdio-1.0
  hs-source-dirs: #{takeDirectory testMain}
  main-is: #{takeFileName testMain}
  build-depends:
      #{intercalate "\n    , " $ sort testDependencies}
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
version: #{renderVersion packageVersion}
build-type: Simple
cabal-version: >= 1.10

#{renderLibrary packageLibrary}
#{renderTests packageTests}
|]

renderVersion :: [Int] -> String
renderVersion = intercalate "." . map show

renderLibrary :: Library -> String
renderLibrary Library{..} = stripEmptyLines [i|
library
  hs-source-dirs: src
  exposed-modules:
#{intercalate "\n" . map ("      " ++) $ libraryExposedModules}
  build-depends:
      #{intercalate "\n    , " $ sort libraryDependencies}
  default-language: Haskell2010
|]

