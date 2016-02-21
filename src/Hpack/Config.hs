{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, package
, section
, Package(..)
, Dependency(..)
, AddSource(..)
, GitUrl
, GitRef
, packageDependencies
, GhcOption
, Section(..)
, Library(..)
, Executable(..)
, SourceRepository(..)
#ifdef TEST
, getModules
, determineModules
#endif
) where

import           Control.Applicative
import           Control.Monad.Compat
import           Data.Aeson.Types
import           Data.Data
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import           Data.List (nub, (\\), sortBy)
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Prelude ()
import           Prelude.Compat
import           System.Directory
import           System.FilePath

import           Hpack.GenericsUtil
import           Hpack.Util
import           Hpack.Yaml

package :: String -> String -> Package
package name version = Package name version Nothing Nothing Nothing Nothing Nothing Nothing [] [] [] Nothing Nothing Nothing [] [] Nothing Nothing [] [] []

section :: a -> Section a
section a = Section a [] [] [] [] [] [] []

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
hyphenize name =
#if MIN_VERSION_aeson(0,10,0)
  camelTo2
#else
  camelTo
#endif
  '-' . drop (length name)

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
, commonOptionsOtherExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions :: Maybe (List GhcProfOption)
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
, packageConfigTestedWith :: Maybe String
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigLibrary :: Maybe (CaptureUnknownFields (Section LibrarySection))
, packageConfigExecutables :: Maybe (HashMap String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigTests :: Maybe (HashMap String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigBenchmarks :: Maybe (HashMap String (CaptureUnknownFields (Section ExecutableSection)))
} deriving (Eq, Show, Generic)

instance HasFieldNames PackageConfig

packageDependencies :: Package -> [Dependency]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . dependencyName)) $
     (concatMap sectionDependencies packageExecutables)
  ++ (concatMap sectionDependencies packageTests)
  ++ (concatMap sectionDependencies packageBenchmarks)
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
  r <- decodeYaml file
  case r of
    Left err -> return (Left err)
    Right config -> do
      dir <- takeDirectory <$> canonicalizePath file
      Right <$> mkPackage dir config

data Dependency = Dependency {
  dependencyName :: String
, dependencyGitRef :: Maybe AddSource
} deriving (Eq, Show, Ord, Generic)

instance IsString Dependency where
  fromString name = Dependency name Nothing

instance FromJSON Dependency where
  parseJSON v = case v of
    String _ -> fromString <$> parseJSON v
    Object o -> addSourceDependency o
    _ -> typeMismatch "String or an Object" v
    where
      addSourceDependency o = Dependency <$> name <*> (Just <$> (local <|> git))
        where
          name :: Parser String
          name = o .: "name"

          git :: Parser AddSource
          git = GitRef <$> url <*> ref

          local :: Parser AddSource
          local = Local <$> o .: "path"

          url :: Parser String
          url =
                ((githubBaseUrl ++) <$> o .: "github")
            <|> (o .: "git")
            <|> fail "neither key \"git\" nor key \"github\" present"

          ref :: Parser String
          ref = o .: "ref"

data AddSource = GitRef GitUrl GitRef | Local FilePath
  deriving (Eq, Show, Ord)

type GitUrl = String
type GitRef = String

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
, packageTestedWith :: Maybe String
, packageExtraSourceFiles :: [FilePath]
, packageDataFiles :: [FilePath]
, packageSourceRepository :: Maybe SourceRepository
, packageLibrary :: Maybe (Section Library)
, packageExecutables :: [Section Executable]
, packageTests :: [Section Executable]
, packageBenchmarks :: [Section Executable]
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
, sectionOtherExtensions :: [String]
, sectionGhcOptions :: [GhcOption]
, sectionGhcProfOptions :: [GhcProfOption]
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

mkPackage :: FilePath -> (CaptureUnknownFields (Section PackageConfig)) -> IO ([String], Package)
mkPackage dir (CaptureUnknownFields unknownFields globalOptions@Section{sectionData = PackageConfig{..}}) = do
  let name = fromMaybe (takeBaseName dir) packageConfigName

  mLibrary <- mapM (toLibrary dir name globalOptions) mLibrarySection
  executables <- toExecutables dir globalOptions (map (fmap captureUnknownFieldsValue) executableSections)
  tests <- toExecutables dir globalOptions (map (fmap captureUnknownFieldsValue) testsSections)
  benchmarks <- toExecutables dir globalOptions  (map (fmap captureUnknownFieldsValue) benchmarkSections)

  licenseFileExists <- doesFileExist (dir </> "LICENSE")

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist . (dir </>)) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs executables
    ++ concatMap sectionSourceDirs tests
    ++ concatMap sectionSourceDirs benchmarks
    )

  (extraSourceFilesWarnings, extraSourceFiles) <-
    expandGlobs dir (fromMaybeList packageConfigExtraSourceFiles)

  (dataFilesWarnings, dataFiles) <-
    expandGlobs dir (fromMaybeList packageConfigDataFiles)

  let pkg = Package {
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
      , packageTestedWith = packageConfigTestedWith
      , packageExtraSourceFiles = extraSourceFiles
      , packageDataFiles = dataFiles
      , packageSourceRepository = sourceRepository
      , packageLibrary = mLibrary
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary)
        ++ formatUnknownSectionFields "executable" executableSections
        ++ formatUnknownSectionFields "test" testsSections
        ++ formatMissingSourceDirs missingSourceDirs
        ++ extraSourceFilesWarnings
        ++ dataFilesWarnings

  return (warnings, pkg)
  where
    executableSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    executableSections = toList packageConfigExecutables

    testsSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    testsSections = toList packageConfigTests

    benchmarkSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    benchmarkSections = toList packageConfigBenchmarks

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
        f (sect, fields) = formatUnknownFields (sectionType ++ " section " ++ show sect) fields

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

toLibrary :: FilePath -> String -> Section global -> Section LibrarySection -> IO (Section Library)
toLibrary dir name globalOptions library = traverse fromLibrarySection sect
  where
    sect :: Section LibrarySection
    sect = mergeSections globalOptions library

    sourceDirs :: [FilePath]
    sourceDirs = sectionSourceDirs sect

    fromLibrarySection :: LibrarySection -> IO Library
    fromLibrarySection LibrarySection{..} = do
      modules <- concat <$> mapM (getModules dir) sourceDirs
      let (exposedModules, otherModules) = determineModules name modules librarySectionExposedModules librarySectionOtherModules
      return (Library exposedModules otherModules)

toExecutables :: FilePath -> Section global -> [(String, Section ExecutableSection)] -> IO [Section Executable]
toExecutables dir globalOptions executables = mapM toExecutable sections
  where
    sections :: [(String, Section ExecutableSection)]
    sections = map (fmap $ mergeSections globalOptions) executables

    toExecutable :: (String, Section ExecutableSection) -> IO (Section Executable)
    toExecutable (name, sect@Section{..}) = do
      (executable, ghcOptions) <- fromExecutableSection sectionData
      return (sect {sectionData = executable, sectionGhcOptions = sectionGhcOptions ++ ghcOptions})
      where
        fromExecutableSection :: ExecutableSection -> IO (Executable, [GhcOption])
        fromExecutableSection ExecutableSection{..} = do
          modules <- maybe (filterMain . concat <$> mapM (getModules dir) sectionSourceDirs) (return . fromList) executableSectionOtherModules
          return (Executable name mainSrcFile modules, ghcOptions)
          where
            filterMain :: [String] -> [String]
            filterMain = maybe id (filter . (/=)) (toModule $ splitDirectories executableSectionMain)

            (mainSrcFile, ghcOptions) = parseMain executableSectionMain

mergeSections :: Section global -> Section a -> Section a
mergeSections globalOptions options
  = Section a sourceDirs dependencies defaultExtensions otherExtensions ghcOptions ghcProfOptions cppOptions
  where
    a = sectionData options
    sourceDirs = sectionSourceDirs globalOptions ++ sectionSourceDirs options
    defaultExtensions = sectionDefaultExtensions globalOptions ++ sectionDefaultExtensions options
    otherExtensions = sectionOtherExtensions globalOptions ++ sectionOtherExtensions options
    ghcOptions = sectionGhcOptions globalOptions ++ sectionGhcOptions options
    ghcProfOptions = sectionGhcProfOptions globalOptions ++ sectionGhcProfOptions options
    cppOptions = sectionCppOptions globalOptions ++ sectionCppOptions options
    dependencies = sectionDependencies globalOptions ++ sectionDependencies options

toSection :: a -> CommonOptions -> Section a
toSection a CommonOptions{..}
  = Section a sourceDirs dependencies defaultExtensions otherExtensions ghcOptions ghcProfOptions cppOptions
  where
    sourceDirs = fromMaybeList commonOptionsSourceDirs
    defaultExtensions = fromMaybeList commonOptionsDefaultExtensions
    otherExtensions = fromMaybeList commonOptionsOtherExtensions
    ghcOptions = fromMaybeList commonOptionsGhcOptions
    ghcProfOptions = fromMaybeList commonOptionsGhcProfOptions
    cppOptions = fromMaybeList commonOptionsCppOptions
    dependencies = fromMaybeList commonOptionsDependencies

pathsModuleFromPackageName :: String -> String
pathsModuleFromPackageName name = "Paths_" ++ map f name
  where
    f '-' = '_'
    f x = x

determineModules :: String -> [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules name modules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (modules, [])
  _ -> (exposedModules, otherModules)
  where
    otherModules   = maybe ((modules \\ exposedModules) ++ pathsModule) fromList mOtherModules
    exposedModules = maybe (modules \\ otherModules)   fromList mExposedModules
    pathsModule = [pathsModuleFromPackageName name] \\ exposedModules

getModules :: FilePath -> FilePath -> IO [String]
getModules dir src_ = sort <$> do
  exists <- doesDirectoryExist (dir </> src_)
  if exists
    then do
      src <- canonicalizePath (dir </> src_)
      removeSetup src . toModules <$> getFilesRecursive src
    else return []
  where
    toModules :: [[FilePath]] -> [String]
    toModules = catMaybes . map toModule

    removeSetup :: FilePath -> [String] -> [String]
    removeSetup src
      | src == dir = filter (/= "Setup")
      | otherwise = id

fromMaybeList :: Maybe (List a) -> [a]
fromMaybeList = maybe [] fromList
