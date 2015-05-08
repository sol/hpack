{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Config (
  readPackageConfig
, Package(..)
, Dependency
, GhcOption
, Library(..)
, Executable(..)
) where

import           Prelude ()
import           Prelude.Compat
import           Control.Applicative
import           Control.Monad.Compat
import           Data.List (sort, (\\))
import           Data.String
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as T
import           System.FilePath
import           System.Directory
import           Data.Data
import           Data.Aeson.Types

import           Util

genericParseJSON_ :: forall a. (Typeable a, Generic a, GFromJSON (Rep a)) => Value -> Parser a
genericParseJSON_ = genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

hyphenize :: String -> String -> String
hyphenize name = camelTo '-' . drop (length name)

data CaptureUnknownFields a = CaptureUnknownFields {captureUnknownFieldsFields :: [String], captureUnknownFieldsValue :: a}
  deriving (Eq, Show, Generic, Data, Typeable)

instance (Data a, FromJSON a) => FromJSON (CaptureUnknownFields a) where
  parseJSON v = captureUnknownFields <$> parseJSON v
    where
      captureUnknownFields a = case v of
        Object o -> CaptureUnknownFields unknown a
          where
            unknown = keys \\ fields
            keys = map T.unpack (Map.keys o)
            constr = toConstr a
            name = showConstr constr
            fields = map (hyphenize name) (constrFields constr)
        _ -> CaptureUnknownFields [] a

data LibrarySection = LibrarySection {
  librarySectionSourceDirs :: Maybe (List FilePath)
, librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
, librarySectionDependencies :: Maybe (List Dependency)
, librarySectionDefaultExtensions :: Maybe (List String)
, librarySectionGhcOptions :: Maybe (List GhcOption)
} deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_

data ExecutableSection = ExecutableSection {
  executableSectionMain :: FilePath
, executableSectionSourceDirs :: Maybe (List FilePath)
, executableSectionOtherModules :: Maybe (List String)
, executableSectionDependencies :: Maybe (List Dependency)
, executableSectionDefaultExtensions :: Maybe (List String)
, executableSectionGhcOptions :: Maybe (List GhcOption)
} deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_

data PackageConfig = PackageConfig {
  packageConfigName :: Maybe String
, packageConfigVersion :: Maybe String
, packageConfigSynopsis :: Maybe String
, packageConfigDescription :: Maybe String
, packageConfigHomepage :: Maybe (Maybe String)
, packageConfigBugReports :: Maybe (Maybe String)
, packageConfigCategory :: Maybe String
, packageConfigStability :: Maybe String
, packageConfigAuthor :: Maybe (List String)
, packageConfigMaintainer :: Maybe (List String)
, packageConfigCopyright :: Maybe (List String)
, packageConfigLicense :: Maybe String
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe String
, packageConfigSourceDirs :: Maybe (List FilePath)
, packageConfigDependencies :: Maybe (List Dependency)
, packageConfigDefaultExtensions :: Maybe (List String)
, packageConfigGhcOptions :: Maybe (List GhcOption)
, packageConfigLibrary :: Maybe (CaptureUnknownFields LibrarySection)
, packageConfigExecutables :: Maybe (HashMap String (CaptureUnknownFields ExecutableSection))
, packageConfigTests :: Maybe (HashMap String (CaptureUnknownFields ExecutableSection))
} deriving (Eq, Show, Generic, Data, Typeable)

instance FromJSON PackageConfig where
  parseJSON value = handleNullValues <$> genericParseJSON_ value
    where
      handleNullValues :: PackageConfig -> PackageConfig
      handleNullValues =
          ifNull "homepage" (\p -> p {packageConfigHomepage = Just Nothing})
        . ifNull "bug-reports" (\p -> p {packageConfigBugReports = Just Nothing})

      ifNull :: String -> (a -> a) -> a -> a
      ifNull name f
        | isNull name value = f
        | otherwise = id

isNull :: String -> Value -> Bool
isNull name value = case parseMaybe p value of
  Just Null -> True
  _ -> False
  where
    p = parseJSON >=> (.: fromString name)

readPackageConfig :: FilePath -> IO (Either String ([String], Package))
readPackageConfig file = do
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
, packageHomepage :: Maybe String
, packageBugReports :: Maybe String
, packageCategory :: Maybe String
, packageStability :: Maybe String
, packageAuthor :: [String]
, packageMaintainer :: [String]
, packageCopyright :: [String]
, packageLicense :: Maybe String
, packageLicenseFile :: Maybe FilePath
, packageExtraSourceFiles :: [FilePath]
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
, libraryDefaultExtensions :: [String]
, libraryGhcOptions :: [GhcOption]
} deriving (Eq, Show)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableSourceDirs :: [FilePath]
, executableOtherModules :: [String]
, executableDependencies :: [[Dependency]]
, executableDefaultExtensions :: [String]
, executableGhcOptions :: [GhcOption]
} deriving (Eq, Show)

mkPackage :: (CaptureUnknownFields PackageConfig) -> IO ([String], Package)
mkPackage (CaptureUnknownFields unknownFields PackageConfig{..}) = do
  let dependencies = fromMaybeList packageConfigDependencies
  let sourceDirs = fromMaybeList packageConfigSourceDirs
  let defaultExtensions = fromMaybeList packageConfigDefaultExtensions
  let ghcOptions = fromMaybeList packageConfigGhcOptions
  mLibrary <- mapM (mkLibrary sourceDirs dependencies defaultExtensions ghcOptions) (captureUnknownFieldsValue <$> packageConfigLibrary)
  executables <- toExecutables sourceDirs dependencies defaultExtensions ghcOptions (fmap captureUnknownFieldsValue <$> packageConfigExecutables)
  tests <- toExecutables sourceDirs dependencies defaultExtensions ghcOptions (fmap captureUnknownFieldsValue <$> packageConfigTests)

  name <- maybe (takeBaseName <$> getCurrentDirectory) return packageConfigName

  licenseFileExists <- doesFileExist "LICENSE"

  let package = Package {
        packageName = name
      , packageVersion = fromMaybe "0.0.0" packageConfigVersion
      , packageSynopsis = packageConfigSynopsis
      , packageDescription = packageConfigDescription
      , packageHomepage = homepage
      , packageBugReports = bugReports
      , packageCategory = packageConfigCategory
      , packageStability = packageConfigStability
      , packageAuthor = fromMaybeList packageConfigAuthor
      , packageMaintainer = fromMaybeList packageConfigMaintainer
      , packageCopyright = fromMaybeList packageConfigCopyright
      , packageLicense = packageConfigLicense
      , packageLicenseFile = guard licenseFileExists >> Just "LICENSE"
      , packageExtraSourceFiles = fromMaybeList packageConfigExtraSourceFiles
      , packageSourceRepository = github
      , packageLibrary = mLibrary
      , packageExecutables = executables
      , packageTests = tests
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary)
        ++ formatUnknownSectionFields "executable" packageConfigExecutables
        ++ formatUnknownSectionFields "test" packageConfigTests

  return (warnings, package)
  where
    formatUnknownFields name = map f . sort
      where
        f field = "Ignoring unknown field " ++ show field ++ " in " ++ name

    formatUnknownSectionFields sectionType = maybe [] (concatMap f . Map.toList . fmap captureUnknownFieldsFields)
      where
        f :: (String, [String]) -> [String]
        f (section, fields) = formatUnknownFields (sectionType ++ " section " ++ show section) fields

    github = ("https://github.com/" ++) <$> packageConfigGithub

    homepage :: Maybe String
    homepage = case packageConfigHomepage of
      Just Nothing -> Nothing
      _ -> join packageConfigHomepage <|> fromGithub
      where
        fromGithub = ((++ "#readme") <$> github)

    bugReports :: Maybe String
    bugReports = case packageConfigBugReports of
      Just Nothing -> Nothing
      _ -> join packageConfigBugReports <|> fromGithub
      where
        fromGithub = ((++ "/issues") <$> github)

mkLibrary :: [FilePath] -> [Dependency] -> [String] -> [GhcOption] -> LibrarySection -> IO Library
mkLibrary globalSourceDirs globalDependencies globalDefaultExtensions globalGhcOptions LibrarySection{..} = do
  modules <- concat <$> mapM getModules sourceDirs

  let (exposedModules, otherModules) = determineModules modules librarySectionExposedModules librarySectionOtherModules

  return (Library sourceDirs exposedModules otherModules dependencies defaultExtensions ghcOptions)
  where
    sourceDirs = globalSourceDirs ++ fromMaybeList librarySectionSourceDirs
    dependencies = filter (not . null) [globalDependencies, fromMaybeList librarySectionDependencies]
    defaultExtensions = globalDefaultExtensions ++ fromMaybeList librarySectionDefaultExtensions
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
    toModules :: [[FilePath]] -> [String]
    toModules = catMaybes . map toModule

toExecutables :: [FilePath] -> [Dependency] -> [String] -> [GhcOption] -> Maybe (HashMap String ExecutableSection) -> IO [Executable]
toExecutables globalSourceDirs globalDependencies globalDefaultExtensions globalGhcOptions executables = (mapM toExecutable . Map.toList) (fromMaybe mempty executables)
  where
    toExecutable (name, ExecutableSection{..}) = do
      modules <- maybe (filterMain . concat <$> mapM getModules sourceDirs) (return . fromList) executableSectionOtherModules
      return $ Executable name executableSectionMain sourceDirs modules dependencies defaultExtensions ghcOptions
      where
        dependencies = filter (not . null) [globalDependencies, fromMaybeList executableSectionDependencies]
        sourceDirs = globalSourceDirs ++ fromMaybeList executableSectionSourceDirs
        defaultExtensions = globalDefaultExtensions ++ fromMaybeList executableSectionDefaultExtensions
        ghcOptions = globalGhcOptions ++ fromMaybeList executableSectionGhcOptions

        filterMain :: [String] -> [String]
        filterMain = maybe id (filter . (/=)) (toModule $ splitDirectories executableSectionMain)

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
