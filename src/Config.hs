{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Config (
  readConfig
, Package(..)
, Library(..)
, Executable(..)
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

data ExecutableSection = ExecutableSection {
  executableSectionMain :: FilePath
, executableSectionDependencies :: Maybe (List Dependency)
} deriving (Eq, Show, Generic)

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_ "ExecutableSection"

data ConfigFile = ConfigFile {
  configFileName :: String
, configFileDependencies :: Maybe [Dependency]
, configFileExecutables :: Maybe (HashMap String ExecutableSection)
, configFileTests :: Maybe (HashMap String ExecutableSection)
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
, packageExecutables :: [Executable]
, packageTests :: [Executable]
} deriving (Eq, Show)

data Library = Library {
  libraryExposedModules :: [String]
, libraryDependencies :: [Dependency]
} deriving (Eq, Show)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableDependencies :: [Dependency]
} deriving (Eq, Show)

mkPackage :: ConfigFile -> IO Package
mkPackage ConfigFile{..} = do
  let dependencies = fromMaybe [] configFileDependencies
  library <- mkLibrary dependencies
  let package = Package {
        packageName = configFileName
      , packageVersion = [0,0,0]
      , packageLibrary = library
      , packageExecutables = toExecutables dependencies configFileExecutables
      , packageTests       = toExecutables dependencies configFileTests
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

toExecutables :: [Dependency] -> Maybe (HashMap String ExecutableSection) -> [Executable]
toExecutables dependencies executables = (map (uncurry $ toExecutable dependencies) . Map.toList) (fromMaybe mempty executables)

toExecutable :: [Dependency] -> String -> ExecutableSection -> Executable
toExecutable dependencies name t = Executable name (executableSectionMain t) (dependencies ++ maybe [] fromList (executableSectionDependencies t))
