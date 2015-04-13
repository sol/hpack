{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module Config (
  readConfig
, Package(..)
, Dependency
, GhcOption
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
import           System.FilePath
import           System.Directory

import           Util

data LibrarySection = LibrarySection {
  librarySectionSourceDirs :: Maybe (List FilePath)
, librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
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
  configFileName :: Maybe String
, configFileVersion :: Maybe String
, configFileSynopsis :: Maybe String
, configFileDescription :: Maybe String
, configFileCategory :: Maybe String
, configFileAuthor :: Maybe String
, configFileMaintainer :: Maybe String
, configFileCopyright :: Maybe String
, configFileLicense :: Maybe String
, configFileGithub :: Maybe String
, configFileSourceDirs :: Maybe (List FilePath)
, configFileDependencies :: Maybe (List Dependency)
, configFileGhcOptions :: Maybe (List GhcOption)
, configFileLibrary :: Maybe LibrarySection
, configFileExecutables :: Maybe (HashMap String ExecutableSection)
, configFileTests :: Maybe (HashMap String ExecutableSection)
} deriving (Eq, Show, Generic)

instance FromJSON ConfigFile where
  parseJSON = genericParseJSON_ "ConfigFile"

readConfig :: FilePath -> IO (Either String Package)
readConfig file = do
  config <- decodeFileEither file
  either (return . Left . errToString) (fmap Right . mkPackage) config
  where
    errToString err = file ++ case err of
      AesonException e -> ": " ++ e
      InvalidYaml (Just e) -> let loc = yamlProblemMark e in ":" ++ show (yamlLine loc) ++ ":" ++ show (yamlColumn loc) ++ ": " ++ yamlProblem e ++ " " ++ yamlContext e
      _ -> ": " ++ show err

type Dependency = String
type GhcOption = String

data Package = Package {
  packageName :: String
, packageVersion :: String
, packageSynopsis :: Maybe String
, packageDescription :: Maybe String
, packageCategory :: Maybe String
, packageAuthor :: Maybe String
, packageMaintainer :: Maybe String
, packageCopyright :: Maybe String
, packageLicense :: Maybe String
, packageLicenseFile :: Maybe FilePath
, packageSourceRepository :: Maybe String
, packageLibrary :: Maybe Library
, packageExecutables :: [Executable]
, packageTests :: [Executable]
} deriving (Eq, Show)

data Library = Library {
  librarySourceDirs :: [FilePath]
, libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryDependencies :: [[Dependency]]
, libraryGhcOptions :: [GhcOption]
} deriving (Eq, Show)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableSourceDirs :: [FilePath]
, executableDependencies :: [[Dependency]]
, executableGhcOptions :: [GhcOption]
} deriving (Eq, Show)

mkPackage :: ConfigFile -> IO Package
mkPackage ConfigFile{..} = do
  let dependencies = fromMaybeList configFileDependencies
  let sourceDirs = fromMaybeList configFileSourceDirs
  let ghcOptions = fromMaybeList configFileGhcOptions
  mLibrary <- mapM (mkLibrary sourceDirs dependencies ghcOptions) configFileLibrary

  name <- maybe (takeBaseName <$> getCurrentDirectory) return configFileName

  licenseFileExists <- doesFileExist "LICENSE"

  let package = Package {
        packageName = name
      , packageVersion = fromMaybe "0.0.0" configFileVersion
      , packageSynopsis = configFileSynopsis
      , packageDescription = configFileDescription
      , packageCategory = configFileCategory
      , packageAuthor = configFileAuthor
      , packageMaintainer = configFileMaintainer
      , packageCopyright = configFileCopyright
      , packageLicense = configFileLicense
      , packageLicenseFile = guard licenseFileExists >> Just "LICENSE"
      , packageSourceRepository = ("https://github.com/" ++) <$> configFileGithub
      , packageLibrary = mLibrary
      , packageExecutables = toExecutables sourceDirs dependencies ghcOptions configFileExecutables
      , packageTests       = toExecutables sourceDirs dependencies ghcOptions configFileTests
      }
  return package

mkLibrary :: [FilePath] -> [Dependency] -> [GhcOption] -> LibrarySection -> IO Library
mkLibrary globalSourceDirs globalDependencies globalGhcOptions LibrarySection{..} = do
  modules <- getModules "src"

  let (exposedModules, otherModules) = determineModules modules librarySectionExposedModules librarySectionOtherModules

  return (Library sourceDirs exposedModules otherModules dependencies ghcOptions)
  where
    sourceDirs = globalSourceDirs ++ fromMaybeList librarySectionSourceDirs
    dependencies = filter (not . null) [globalDependencies, fromMaybeList librarySectionDependencies]
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

toExecutables :: [FilePath] -> [Dependency] -> [GhcOption] -> Maybe (HashMap String ExecutableSection) -> [Executable]
toExecutables globalSourceDirs globalDependencies globalGhcOptions executables = (map toExecutable . Map.toList) (fromMaybe mempty executables)
  where
    toExecutable (name, ExecutableSection{..}) = Executable name executableSectionMain sourceDirs dependencies ghcOptions
      where
        dependencies = filter (not . null) [globalDependencies, fromMaybeList executableSectionDependencies]
        sourceDirs = globalSourceDirs ++ fromMaybeList executableSectionSourceDirs
        ghcOptions = globalGhcOptions ++ fromMaybeList executableSectionGhcOptions

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
