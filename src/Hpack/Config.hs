{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, Package(..)
, Dependency(..)
, GitRef(..)
, packageDependencies
, GhcOption
, Section(..)
, Library(..)
, Executable(..)
, SourceRepository(..)
) where

import           Control.Applicative
import           Control.Monad.Compat
import           Data.Aeson.Types
import           Data.Data
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Ord
import           Data.List (nub, (\\), sortBy)
import           Data.Maybe
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Yaml
import           GHC.Generics
import           Prelude ()
import           Prelude.Compat
import           System.Directory
import           System.FilePath

import           Hpack.Util
import           Hpack.GenericsUtil

packageConfig :: FilePath
packageConfig = "package.yaml"

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

genericParseJSON_ :: forall a. (Generic a, GFromJSON (Rep a), HasTypeName a) => Value -> Parser a
genericParseJSON_ = genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name :: String
    name = typeName (Proxy :: Proxy a)

hyphenize :: String -> String -> String
hyphenize name = camelTo '-' . drop (length name)

class HasFieldNames a where
  fieldNames :: Proxy a -> [String]

  default fieldNames :: (HasTypeName a, Generic a, Selectors (Rep a)) => Proxy a -> [String]
  fieldNames proxy = map (hyphenize $ typeName proxy) (selectors proxy)

data CaptureUnknownFields a = CaptureUnknownFields {
  captureUnknownFieldsFields :: [String]
, captureUnknownFieldsValue :: a
} deriving (Eq, Show, Generic)

instance (HasFieldNames a, FromJSON a) => FromJSON (CaptureUnknownFields a) where
  parseJSON v = captureUnknownFields <$> parseJSON v
    where
      captureUnknownFields a = case v of
        Object o -> CaptureUnknownFields unknown a
          where
            unknown = keys \\ fields
            keys = map T.unpack (Map.keys o)
            fields = fieldNames (Proxy :: Proxy a)
        _ -> CaptureUnknownFields [] a

data LibrarySection = LibrarySection {
  librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

instance HasFieldNames LibrarySection

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_

data ExecutableSection = ExecutableSection {
  executableSectionMain :: FilePath
, executableSectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

instance HasFieldNames ExecutableSection

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_

data CommonOptions = CommonOptions {
  commonOptionsSourceDirs :: Maybe (List FilePath)
, commonOptionsDependencies :: Maybe (List Dependency)
, commonOptionsDefaultExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsCppOptions :: Maybe (List CppOption)
} deriving (Eq, Show, Generic)

instance HasFieldNames CommonOptions

instance FromJSON CommonOptions where
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
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigLibrary :: Maybe (CaptureUnknownFields (Section LibrarySection))
, packageConfigExecutables :: Maybe (HashMap String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigTests :: Maybe (HashMap String (CaptureUnknownFields (Section ExecutableSection)))
} deriving (Eq, Show, Generic)

instance HasFieldNames PackageConfig

packageDependencies :: Package -> [Dependency]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . dependencyName)) $
     (concatMap sectionDependencies packageExecutables)
  ++ (concatMap sectionDependencies packageTests)
  ++ maybe [] sectionDependencies packageLibrary

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
      InvalidYaml (Just (YamlException s)) -> ": " ++ s
      InvalidYaml (Just (YamlParseException{..})) -> ":" ++ show yamlLine ++ ":" ++ show yamlColumn ++ ": " ++ yamlProblem ++ " " ++ yamlContext
        where YamlMark{..} = yamlProblemMark
      _ -> ": " ++ show err

data Dependency = Dependency {
  dependencyName :: String
, dependencyGitRef :: Maybe GitRef
} deriving (Eq, Show, Ord, Generic)

instance IsString Dependency where
  fromString name = Dependency name Nothing

instance FromJSON Dependency where
  parseJSON v = case v of
    String _ -> fromString <$> parseJSON v
    Object o -> gitDependency o
    _ -> typeMismatch "String or an Object" v
    where
      gitDependency o = Dependency <$> name <*> (Just <$> git)
        where
          name :: Parser String
          name = o .: "name"

          git :: Parser GitRef
          git = GitRef <$> url <*> ref

          url :: Parser String
          url =
                ((githubBaseUrl ++) <$> o .: "github")
            <|> (o .: "git")
            <|> fail "neither key \"git\" nor key \"github\" present"

          ref :: Parser String
          ref = o .: "ref"

data GitRef = GitRef {
  gitRefUrl :: String
, gitRefRef :: String
} deriving (Eq, Show, Ord, Generic)

type GhcOption = String
type CppOption = String

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
, packageDataFiles :: [FilePath]
, packageSourceRepository :: Maybe SourceRepository
, packageLibrary :: Maybe (Section Library)
, packageExecutables :: [Section Executable]
, packageTests :: [Section Executable]
} deriving (Eq, Show)

data Library = Library {
  libraryExposedModules :: [String]
, libraryOtherModules :: [String]
} deriving (Eq, Show)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableOtherModules :: [String]
} deriving (Eq, Show)

data Section a = Section {
  sectionData :: a
, sectionSourceDirs :: [FilePath]
, sectionDependencies :: [Dependency]
, sectionDefaultExtensions :: [String]
, sectionGhcOptions :: [GhcOption]
, sectionCppOptions :: [CppOption]
} deriving (Eq, Show, Functor, Foldable, Traversable)

instance HasFieldNames a => HasFieldNames (Section a) where
  fieldNames Proxy = fieldNames (Proxy :: Proxy a) ++ fieldNames (Proxy :: Proxy CommonOptions)

instance FromJSON a => FromJSON (Section a) where
  parseJSON v = toSection <$> parseJSON v <*> parseJSON v

data SourceRepository = SourceRepository {
  sourceRepositoryUrl :: String
, sourceRepositorySubdir :: Maybe String
} deriving (Eq, Show)

mkPackage :: (CaptureUnknownFields (Section PackageConfig)) -> IO ([String], Package)
mkPackage (CaptureUnknownFields unknownFields globalOptions@Section{sectionData = PackageConfig{..}}) = do
  mLibrary <- mapM (toLibrary globalOptions) mLibrarySection
  executables <- toExecutables globalOptions (map (fmap captureUnknownFieldsValue) executableSections)
  tests <- toExecutables globalOptions (map (fmap captureUnknownFieldsValue) testsSections)

  name <- maybe (takeBaseName <$> getCurrentDirectory) return packageConfigName

  licenseFileExists <- doesFileExist "LICENSE"

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs executables
    ++ concatMap sectionSourceDirs tests
    )

  (extraSourceFilesWarnings, extraSourceFiles) <-
    expandGlobs (fromMaybeList packageConfigExtraSourceFiles)

  (dataFilesWarnings, dataFiles) <-
    expandGlobs (fromMaybeList packageConfigDataFiles)

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
      , packageExtraSourceFiles = extraSourceFiles
      , packageDataFiles = dataFiles
      , packageSourceRepository = sourceRepository
      , packageLibrary = mLibrary
      , packageExecutables = executables
      , packageTests = tests
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary)
        ++ formatUnknownSectionFields "executable" executableSections
        ++ formatUnknownSectionFields "test" testsSections
        ++ formatMissingSourceDirs missingSourceDirs
        ++ extraSourceFilesWarnings
        ++ dataFilesWarnings

  return (warnings, package)
  where
    executableSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    executableSections = toList packageConfigExecutables

    testsSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    testsSections = toList packageConfigTests

    toList :: Maybe (HashMap String a) -> [(String, a)]
    toList = Map.toList . fromMaybe mempty

    mLibrarySection :: Maybe (Section LibrarySection)
    mLibrarySection = captureUnknownFieldsValue <$> packageConfigLibrary

    formatUnknownFields :: String -> [String] -> [String]
    formatUnknownFields name = map f . sort
      where
        f field = "Ignoring unknown field " ++ show field ++ " in " ++ name

    formatUnknownSectionFields :: String -> [(String, CaptureUnknownFields a)] -> [String]
    formatUnknownSectionFields sectionType = concatMap f . map (fmap captureUnknownFieldsFields)
      where
        f :: (String, [String]) -> [String]
        f (section, fields) = formatUnknownFields (sectionType ++ " section " ++ show section) fields

    formatMissingSourceDirs = map f
      where
        f name = "Specified source-dir " ++ show name ++ " does not exist"

    sourceRepository :: Maybe SourceRepository
    sourceRepository = parseGithub <$> packageConfigGithub
      where
        parseGithub :: Text -> SourceRepository
        parseGithub input = case map T.unpack $ T.splitOn "/" input of
          [user, repo, subdir] ->
            SourceRepository (githubBaseUrl ++ user ++ "/" ++ repo) (Just subdir)
          _ -> SourceRepository (githubBaseUrl ++ T.unpack input) Nothing

    homepage :: Maybe String
    homepage = case packageConfigHomepage of
      Just Nothing -> Nothing
      _ -> join packageConfigHomepage <|> fromGithub
      where
        fromGithub = (++ "#readme") . sourceRepositoryUrl <$> sourceRepository

    bugReports :: Maybe String
    bugReports = case packageConfigBugReports of
      Just Nothing -> Nothing
      _ -> join packageConfigBugReports <|> fromGithub
      where
        fromGithub = (++ "/issues") . sourceRepositoryUrl <$> sourceRepository

toLibrary :: Section global -> Section LibrarySection -> IO (Section Library)
toLibrary globalOptions library = traverse fromLibrarySection section
  where
    section :: Section LibrarySection
    section = mergeSections globalOptions library

    sourceDirs :: [FilePath]
    sourceDirs = sectionSourceDirs section

    fromLibrarySection :: LibrarySection -> IO Library
    fromLibrarySection LibrarySection{..} = do
      modules <- concat <$> mapM getModules sourceDirs
      let (exposedModules, otherModules) = determineModules modules librarySectionExposedModules librarySectionOtherModules
      return (Library exposedModules otherModules)

toExecutables :: Section global -> [(String, Section ExecutableSection)] -> IO [Section Executable]
toExecutables globalOptions executables = mapM toExecutable sections
  where
    sections :: [(String, Section ExecutableSection)]
    sections = map (fmap $ mergeSections globalOptions) executables

    toExecutable :: (String, Section ExecutableSection) -> IO (Section Executable)
    toExecutable (name, section) = traverse fromExecutableSection section
      where
        sourceDirs :: [FilePath]
        sourceDirs = sectionSourceDirs section

        fromExecutableSection :: ExecutableSection -> IO Executable
        fromExecutableSection ExecutableSection{..} = do
          modules <- maybe (filterMain . concat <$> mapM getModules sourceDirs) (return . fromList) executableSectionOtherModules
          return (Executable name executableSectionMain modules)
          where
            filterMain :: [String] -> [String]
            filterMain = maybe id (filter . (/=)) (toModule $ splitDirectories executableSectionMain)

mergeSections :: Section global -> Section a -> Section a
mergeSections globalOptions options
  = Section a sourceDirs dependencies defaultExtensions ghcOptions cppOptions
  where
    a = sectionData options
    sourceDirs = sectionSourceDirs globalOptions ++ sectionSourceDirs options
    defaultExtensions = sectionDefaultExtensions globalOptions ++ sectionDefaultExtensions options
    ghcOptions = sectionGhcOptions globalOptions ++ sectionGhcOptions options
    cppOptions = sectionCppOptions globalOptions ++ sectionCppOptions options
    dependencies = sectionDependencies globalOptions ++ sectionDependencies options

toSection :: a -> CommonOptions -> Section a
toSection a CommonOptions{..}
  = Section a sourceDirs dependencies defaultExtensions ghcOptions cppOptions
  where
    sourceDirs = fromMaybeList commonOptionsSourceDirs
    defaultExtensions = fromMaybeList commonOptionsDefaultExtensions
    ghcOptions = fromMaybeList commonOptionsGhcOptions
    cppOptions = fromMaybeList commonOptionsCppOptions
    dependencies = fromMaybeList commonOptionsDependencies

determineModules :: [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules modules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (modules, [])
  _ -> (exposedModules, otherModules)
  where
    otherModules   = maybe (modules \\ exposedModules) fromList mOtherModules
    exposedModules = maybe (modules \\ otherModules)   fromList mExposedModules

getModules :: FilePath -> IO [String]
getModules src = sort <$> do
  exits <- doesDirectoryExist src
  if exits
    then toModules <$> getFilesRecursive src
    else return []
  where
    toModules :: [[FilePath]] -> [String]
    toModules = catMaybes . map toModule

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
