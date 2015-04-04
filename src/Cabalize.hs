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
  hs-source-dirs: #{takeDirectory executableMain}
  main-is: #{takeFileName executableMain}
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
version: #{renderVersion packageVersion}
build-type: Simple
cabal-version: >= 1.10

#{renderLibrary packageLibrary}
#{renderExecutables packageExecutables}
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

