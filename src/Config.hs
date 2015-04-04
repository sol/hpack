{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Config (
  readConfig
, Package(..)
, Library(..)
, Executable(..)
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad (guard)
import           Data.List ((\\))
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           System.Directory

import           Util

data LibrarySection = LibrarySection {
  librarySectionExposedModules :: Maybe (List Dependency)
, librarySectionOtherModules :: Maybe (List Dependency)
, librarySectionDependencies :: Maybe (List Dependency)
, librarySectionGhcOptions :: Maybe (List GhcOption)
} deriving (Eq, Show, Generic)

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_ "LibrarySection"

data ExecutableSection = ExecutableSection {
  executableSectionMain :: FilePath
, executableSectionSourceDirs :: Maybe (List FilePath)
, executableSectionDependencies :: Maybe (List Dependency)
, executableSectionGhcOptions :: Maybe (List GhcOption)
} deriving (Eq, Show, Generic)

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_ "ExecutableSection"

data ConfigFile = ConfigFile {
  configFileName :: String
, configFileVersion :: Maybe String
, configFileAuthor :: Maybe String
, configFileMaintainer :: Maybe String
, configFileCopyright :: Maybe String
, configFileLicense :: Maybe String
, configFileDependencies :: Maybe [Dependency]
, configFileGhcOptions :: Maybe (List GhcOption)
, configFileLibrary :: Maybe LibrarySection
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
type GhcOption = String

data Package = Package {
  packageName :: String
, packageVersion :: String
, packageAuthor :: Maybe String
, packageMaintainer :: Maybe String
, packageCopyright :: Maybe String
, packageLicense :: Maybe String
, packageLicenseFile :: Maybe FilePath
, packageLibrary :: Maybe Library
, packageExecutables :: [Executable]
, packageTests :: [Executable]
} deriving (Eq, Show)

data Library = Library {
  libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryDependencies :: [Dependency]
, libraryGhcOptions :: [GhcOption]
} deriving (Eq, Show)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableSourceDirs :: [FilePath]
, executableDependencies :: [Dependency]
, executableGhcOptions :: [GhcOption]
} deriving (Eq, Show)

mkPackage :: ConfigFile -> IO Package
mkPackage ConfigFile{..} = do
  let dependencies = fromMaybe [] configFileDependencies
  let ghcOptions = fromMaybeList configFileGhcOptions
  mLibrary <- mapM (mkLibrary dependencies ghcOptions) configFileLibrary

  licenseFileExists <- doesFileExist "LICENSE"

  let package = Package {
        packageName = configFileName
      , packageVersion = fromMaybe "0.0.0" configFileVersion
      , packageAuthor = configFileAuthor
      , packageMaintainer = configFileMaintainer
      , packageCopyright = configFileCopyright
      , packageLicense = configFileLicense
      , packageLicenseFile = guard licenseFileExists >> Just "LICENSE"
      , packageLibrary = mLibrary
      , packageExecutables = toExecutables dependencies ghcOptions configFileExecutables
      , packageTests       = toExecutables dependencies ghcOptions configFileTests
      }
  return package

mkLibrary :: [Dependency] -> [GhcOption] -> LibrarySection -> IO Library
mkLibrary globalDependencies globalGhcOptions LibrarySection{..} = do
  modules <- getModules "src"

  let (exposedModules, otherModules) = determineModules modules librarySectionExposedModules librarySectionOtherModules

  return (Library exposedModules otherModules dependencies ghcOptions)
  where
    dependencies = globalDependencies ++ fromMaybeList librarySectionDependencies
    ghcOptions = globalGhcOptions ++ fromMaybeList librarySectionGhcOptions

determineModules :: [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules modules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (modules, [])
  _ -> (exposedModules, otherModules)
  where
    otherModules   = maybe (modules \\ exposedModules) fromList mOtherModules
    exposedModules = maybe (modules \\ otherModules)   fromList mExposedModules

getModules :: FilePath -> IO [String]
getModules src = do
  exits <- doesDirectoryExist src
  if exits
    then toModules <$> getFilesRecursive src
    else return []
  where
    toModules :: [FilePath] -> [String]
    toModules = catMaybes . map toModule

toExecutables :: [Dependency] -> [GhcOption] -> Maybe (HashMap String ExecutableSection) -> [Executable]
toExecutables dependencies ghcOptions executables = (map (uncurry $ toExecutable dependencies ghcOptions) . Map.toList) (fromMaybe mempty executables)

toExecutable :: [Dependency] -> [GhcOption] -> String -> ExecutableSection -> Executable
toExecutable globalDependencies globalGhcOptions name ExecutableSection{..} = Executable name executableSectionMain sourceDirs dependencies ghcOptions
  where
    dependencies = globalDependencies ++ fromMaybeList executableSectionDependencies
    sourceDirs = fromMaybeList executableSectionSourceDirs
    ghcOptions = globalGhcOptions ++ fromMaybeList executableSectionGhcOptions

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
