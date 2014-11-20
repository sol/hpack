{-# LANGUAGE QuasiQuotes, RecordWildCards #-}
module Cabalize where

import           Control.Applicative
import           Data.Maybe
import           Data.Char
import           Data.List
import           Data.String.Interpolate
import           System.Directory
import           System.FilePath
import           System.Exit.Compat

import           Util
import           Config (Config)
import qualified Config

type Dependency = String

data Package = Package {
  packageName :: String
, packageVersion :: [Int]
, packageLibrary :: Library
} deriving (Eq, Show)

data Library = Library {
  libraryExposedModules :: [String]
, libraryDependencies :: [Dependency]
} deriving (Eq, Show)

configFile :: FilePath
configFile = "package.yaml"

cabalize :: IO (String, FilePath)
cabalize = do
  mConf <- Config.readConfig configFile
  case mConf of
    Just conf -> do
      package <- mkPackage conf
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
|]

renderVersion :: [Int] -> String
renderVersion = intercalate "." . map show

renderLibrary :: Library -> String
renderLibrary Library{..} = dropWhile isSpace [i|
library
  hs-source-dirs: src
  exposed-modules:
#{intercalate "\n" . map ("      " ++) $ libraryExposedModules}
  build-depends:
      #{intercalate "\n    , " $ sort libraryDependencies}
  default-language: Haskell2010
|]

mkPackage :: Config -> IO Package
mkPackage Config.Config{..} = do
  name <- takeBaseName <$> getCurrentDirectory
  library <- mkLibrary dependencies
  let package = Package {
        packageName = name
      , packageVersion = [0,0,0]
      , packageLibrary = library
      }
  return package

mkLibrary :: [Dependency] -> IO Library
mkLibrary dependencies = Library <$> getModules "src" <*> pure dependencies

getModules :: FilePath -> IO [String]
getModules src = toModules <$> getFilesRecursive src
  where
    toModules :: [FilePath] -> [String]
    toModules = catMaybes . map (fmap reverse . stripPrefix (reverse ".hs") . reverse)
