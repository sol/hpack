{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, Package(..)
, Dependency(..)
, GitRef(..)
, packageDependencies
, GhcOption
, Library(..)
, Executable(..)
, SourceRepository(..)
) where

import           Prelude ()
import           Prelude.Compat
import           Control.Applicative
import           Control.Monad.Compat
import           Data.List (nub, sort, (\\))
import           Data.String
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath
import           System.Directory
import           Data.Data
import           Data.Aeson.Types

import           Hpack.Util

packageConfig :: FilePath
packageConfig = "package.yaml"

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

genericParseJSON_ :: forall a. (Typeable a, Generic a, GFromJSON (Rep a)) => Value -> Parser a
genericParseJSON_ = genericParseJSON defaultOptions {fieldLabelModifier = hyphenize name}
  where
    name = (tyConName . typeRepTyCon . typeRep) (Proxy :: Proxy a)

hyphenize :: String -> String -> String
hyphenize name = camelTo '-' . drop (length name)

data CaptureUnknownFields a = CaptureUnknownFields {
  captureUnknownFieldsFields :: [String]
, captureUnknownFieldsValue :: a
} deriving (Eq, Show, Generic, Data, Typeable)

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
, librarySectionCppOptions :: Maybe (List CppOption)
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
, executableSectionCppOptions :: Maybe (List CppOption)
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
, packageConfigGithub :: Maybe Text
, packageConfigSourceDirs :: Maybe (List FilePath)
, packageConfigDependencies :: Maybe (List Dependency)
, packageConfigDefaultExtensions :: Maybe (List String)
, packageConfigGhcOptions :: Maybe (List GhcOption)
, packageConfigCppOptions :: Maybe (List CppOption)
, packageConfigLibrary :: Maybe (CaptureUnknownFields LibrarySection)
, packageConfigExecutables :: Maybe (HashMap String (CaptureUnknownFields ExecutableSection))
, packageConfigTests :: Maybe (HashMap String (CaptureUnknownFields ExecutableSection))
} deriving (Eq, Show, Generic, Data, Typeable)

packageDependencies :: Package -> [Dependency]
packageDependencies Package{..} = nub . sort $
     (concat $ concatMap executableDependencies packageExecutables)
  ++ (concat $ concatMap executableDependencies packageTests)
  ++ maybe [] (concat . libraryDependencies) packageLibrary

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
} deriving (Eq, Show, Ord, Generic, Data, Typeable)

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
} deriving (Eq, Show, Ord, Generic, Data, Typeable)

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
, packageSourceRepository :: Maybe SourceRepository
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
, libraryCppOptions :: [CppOption]
} deriving (Eq, Show)

data Executable = Executable {
  executableName :: String
, executableMain :: FilePath
, executableSourceDirs :: [FilePath]
, executableOtherModules :: [String]
, executableDependencies :: [[Dependency]]
, executableDefaultExtensions :: [String]
, executableGhcOptions :: [GhcOption]
, executableCppOptions :: [CppOption]
} deriving (Eq, Show)

data SourceRepository = SourceRepository {
  sourceRepositoryUrl :: String
, sourceRepositorySubdir :: Maybe String
} deriving (Eq, Show)

mkPackage :: (CaptureUnknownFields PackageConfig) -> IO ([String], Package)
mkPackage (CaptureUnknownFields unknownFields PackageConfig{..}) = do
  let dependencies = fromMaybeList packageConfigDependencies
      sourceDirs = fromMaybeList packageConfigSourceDirs
      defaultExtensions = fromMaybeList packageConfigDefaultExtensions
      ghcOptions = fromMaybeList packageConfigGhcOptions
      cppOptions = fromMaybeList packageConfigCppOptions
      convert f = f sourceDirs dependencies defaultExtensions ghcOptions cppOptions

  mLibrary <- mapM (convert toLibrary) mLibrarySection
  executables <- convert toExecutables mExecutableSections
  tests <- convert toExecutables mTestSections

  name <- maybe (takeBaseName <$> getCurrentDirectory) return packageConfigName

  licenseFileExists <- doesFileExist "LICENSE"

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist) (
       maybe [] librarySourceDirs mLibrary
    ++ concatMap executableSourceDirs executables
    ++ concatMap executableSourceDirs tests
    )

  (extrasWarnings, globbedExtraSourceFiles) <-
    expandGlobs (fromMaybeList packageConfigExtraSourceFiles)

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
      , packageExtraSourceFiles = globbedExtraSourceFiles
      , packageSourceRepository = sourceRepository
      , packageLibrary = mLibrary
      , packageExecutables = executables
      , packageTests = tests
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary)
        ++ formatUnknownSectionFields "executable" packageConfigExecutables
        ++ formatUnknownSectionFields "test" packageConfigTests
        ++ formatMissingSourceDirs missingSourceDirs
        ++ extrasWarnings

  return (warnings, package)
  where
    mLibrarySection :: Maybe LibrarySection
    mLibrarySection = captureUnknownFieldsValue <$> packageConfigLibrary

    mExecutableSections :: Maybe (HashMap String ExecutableSection)
    mExecutableSections = fmap captureUnknownFieldsValue <$> packageConfigExecutables

    mTestSections :: Maybe (HashMap String ExecutableSection)
    mTestSections = fmap captureUnknownFieldsValue <$> packageConfigTests

    formatUnknownFields name = map f . sort
      where
        f field = "Ignoring unknown field " ++ show field ++ " in " ++ name

    formatUnknownSectionFields sectionType = maybe [] (concatMap f . Map.toList . fmap captureUnknownFieldsFields)
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

toLibrary :: [FilePath] -> [Dependency] -> [String] -> [GhcOption] -> [CppOption] -> LibrarySection -> IO Library
toLibrary globalSourceDirs globalDependencies globalDefaultExtensions globalGhcOptions globalCppOptions LibrarySection{..} = do
  modules <- concat <$> mapM getModules sourceDirs

  let (exposedModules, otherModules) = determineModules modules librarySectionExposedModules librarySectionOtherModules

  return (Library sourceDirs exposedModules otherModules dependencies defaultExtensions ghcOptions cppOptions)
  where
    sourceDirs = globalSourceDirs ++ fromMaybeList librarySectionSourceDirs
    dependencies = filter (not . null) [globalDependencies, fromMaybeList librarySectionDependencies]
    defaultExtensions = globalDefaultExtensions ++ fromMaybeList librarySectionDefaultExtensions
    ghcOptions = globalGhcOptions ++ fromMaybeList librarySectionGhcOptions
    cppOptions = globalCppOptions ++ fromMaybeList librarySectionCppOptions

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

toExecutables :: [FilePath] -> [Dependency] -> [String] -> [GhcOption] -> [CppOption] -> Maybe (HashMap String ExecutableSection) -> IO [Executable]
toExecutables globalSourceDirs globalDependencies globalDefaultExtensions globalGhcOptions globalCppOptions executables =
  (mapM toExecutable . Map.toList) (fromMaybe mempty executables)
  where
    toExecutable (name, ExecutableSection{..}) = do
      modules <- maybe (filterMain . concat <$> mapM getModules sourceDirs) (return . fromList) executableSectionOtherModules
      return $ Executable name executableSectionMain sourceDirs modules dependencies defaultExtensions ghcOptions cppOptions
      where
        dependencies = filter (not . null) [globalDependencies, fromMaybeList executableSectionDependencies]
        sourceDirs = globalSourceDirs ++ fromMaybeList executableSectionSourceDirs
        defaultExtensions = globalDefaultExtensions ++ fromMaybeList executableSectionDefaultExtensions
        ghcOptions = globalGhcOptions ++ fromMaybeList executableSectionGhcOptions
        cppOptions = globalCppOptions ++ fromMaybeList executableSectionCppOptions

        filterMain :: [String] -> [String]
        filterMain = maybe id (filter . (/=)) (toModule $ splitDirectories executableSectionMain)

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
