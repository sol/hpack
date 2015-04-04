{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Config (
  readConfig
, Package(..)
, Library(..)
, Test(..)
) where

import           Prelude ()
import           Prelude.Compat

import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           System.Directory

import           Util

data TestSection = TestSection {
  testSectionMain :: FilePath
, testSectionDependencies :: Maybe [String]
} deriving (Eq, Show, Generic)

instance FromJSON TestSection where
  parseJSON = genericParseJSON_ "TestSection"

data ConfigFile = ConfigFile {
  configFileName :: String
, configFileDependencies :: [String]
, configFileTests :: HashMap String TestSection
} deriving (Eq, Show, Generic)

instance FromJSON ConfigFile where
  parseJSON = genericParseJSON_ "ConfigFile"

readConfig :: FilePath -> IO (Maybe Package)
readConfig file = do
  mConfig <- decodeFile file
  case mConfig of
    Just config -> Just <$> mkPackage config
    Nothing -> return Nothing

type Dependency = String

data Package = Package {
  packageName :: String
, packageVersion :: [Int]
, packageLibrary :: Library
, packageTests :: [Test]
} deriving (Eq, Show)

data Library = Library {
  libraryExposedModules :: [String]
, libraryDependencies :: [Dependency]
} deriving (Eq, Show)

data Test = Test {
  testName :: String
, testMain :: FilePath
, testDependencies :: [Dependency]
} deriving (Eq, Show)

mkPackage :: ConfigFile -> IO Package
mkPackage ConfigFile{..} = do
  library <- mkLibrary configFileDependencies
  let package = Package {
        packageName = configFileName
      , packageVersion = [0,0,0]
      , packageLibrary = library
      , packageTests = (map (uncurry $ testConfigToTest configFileDependencies) . Map.toList) configFileTests
      }
  return package

mkLibrary :: [Dependency] -> IO Library
mkLibrary dependencies = Library <$> getModules "src" <*> pure dependencies

getModules :: FilePath -> IO [String]
getModules src = do
  exits <- doesDirectoryExist src
  if exits
    then toModules <$> getFilesRecursive src
    else return []
  where
    toModules :: [FilePath] -> [String]
    toModules = catMaybes . map toModule

testConfigToTest :: [Dependency] -> String -> TestSection -> Test
testConfigToTest dependencies name t = Test name (testSectionMain t) (dependencies ++ fromMaybe [] (testSectionDependencies t))
