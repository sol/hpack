{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, renamePackage
, packageDependencies
, package
, section
, Package(..)
, Dependency(..)
, AddSource(..)
, GitUrl
, GitRef
, GhcOption
, CustomSetup(..)
, Section(..)
, Library(..)
, Executable(..)
, Conditional(..)
, Flag(..)
, SourceRepository(..)
#ifdef TEST
, renameDependencies
, HasFieldNames(..)
, CaptureUnknownFields(..)
, Empty(..)
, getModules
, determineModules
, BuildType(..)
#endif
) where

import           Control.Applicative
import           Control.Monad.Compat
import           Data.Aeson.Types
import           Data.Data
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import           Data.List.Compat (nub, (\\), sortBy, isPrefixOf)
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic, Rep)
import           Prelude ()
import           Prelude.Compat
import           System.Directory
import           System.FilePath

import           Hpack.GenericsUtil
import           Hpack.Util
import           Hpack.Yaml

package :: String -> String -> Package
package name version = Package {
    packageName = name
  , packageVersion = version
  , packageSynopsis = Nothing
  , packageDescription = Nothing
  , packageHomepage = Nothing
  , packageBugReports = Nothing
  , packageCategory = Nothing
  , packageStability = Nothing
  , packageAuthor = []
  , packageMaintainer = []
  , packageCopyright = []
  , packageBuildType = Simple
  , packageLicense = Nothing
  , packageLicenseFile = []
  , packageTestedWith = Nothing
  , packageFlags = []
  , packageExtraSourceFiles = []
  , packageDataFiles = []
  , packageSourceRepository = Nothing
  , packageCustomSetup = Nothing
  , packageLibrary = Nothing
  , packageExecutables = []
  , packageTests = []
  , packageBenchmarks = []
  }

renamePackage :: String -> Package -> Package
renamePackage name p@Package{..} = p {
    packageName = name
  , packageExecutables = map (renameDependencies packageName name) packageExecutables
  , packageTests = map (renameDependencies packageName name) packageTests
  , packageBenchmarks = map (renameDependencies packageName name) packageBenchmarks
  }

renameDependencies :: String -> String -> Section a -> Section a
renameDependencies old new sect@Section{..} = sect {sectionDependencies = map rename sectionDependencies, sectionConditionals = map renameConditional sectionConditionals}
  where
    rename dep
      | dependencyName dep == old = dep {dependencyName = new}
      | otherwise = dep

    renameConditional :: Conditional -> Conditional
    renameConditional (Conditional condition then_ else_) = Conditional condition (renameDependencies old new then_) (renameDependencies old new <$> else_)

packageDependencies :: Package -> [Dependency]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . dependencyName)) $
     (concatMap sectionDependencies packageExecutables)
  ++ (concatMap sectionDependencies packageTests)
  ++ (concatMap sectionDependencies packageBenchmarks)
  ++ maybe [] sectionDependencies packageLibrary

section :: a -> Section a
section a = Section a [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] []

packageConfig :: FilePath
packageConfig = "package.yaml"

githubBaseUrl :: String
githubBaseUrl = "https://github.com/"

#if MIN_VERSION_aeson(1,0,0)
genericParseJSON_ :: forall a. (Generic a, GFromJSON Zero (Rep a), HasTypeName a) => Value -> Parser a
#else
genericParseJSON_ :: forall a. (Generic a, GFromJSON (Rep a), HasTypeName a) => Value -> Parser a
#endif
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
  '-' . drop (length name) . dropWhile (== '_')

type FieldName = String

class HasFieldNames a where
  fieldNames :: Proxy a -> [FieldName]

  default fieldNames :: (HasTypeName a, Selectors (Rep a)) => Proxy a -> [String]
  fieldNames proxy = map (hyphenize $ typeName proxy) (selectors proxy)

  ignoreUnderscoredUnknownFields :: Proxy a -> Bool
  ignoreUnderscoredUnknownFields _ = False

data CaptureUnknownFields a = CaptureUnknownFields {
  captureUnknownFieldsFields :: [FieldName]
, captureUnknownFieldsValue :: a
} deriving (Eq, Show, Generic)

captureUnknownFields :: forall a. (HasFieldNames a, FromJSON a) => Value -> Parser (CaptureUnknownFields a)
captureUnknownFields v = CaptureUnknownFields unknown <$> parseJSON v
  where
    unknown = getUnknownFields v (Proxy :: Proxy a)

instance (HasFieldNames a, FromJSON a) => FromJSON (CaptureUnknownFields (Section a)) where
  parseJSON v = do
    (unknownFields, sect) <- toSection <$> parseJSON v <*> parseJSON v
    return (CaptureUnknownFields (unknownSectionFields ++ unknownFields) sect)
    where
      unknownSectionFields = getUnknownFields v (Proxy :: Proxy (Section a))

instance FromJSON (CaptureUnknownFields CustomSetupSection) where
  parseJSON = captureUnknownFields

instance FromJSON (CaptureUnknownFields FlagSection) where
  parseJSON = captureUnknownFields

getUnknownFields :: forall a. HasFieldNames a => Value -> Proxy a -> [FieldName]
getUnknownFields v _ = case v of
  Object o -> ignoreUnderscored unknown
    where
      unknown = keys \\ fields
      keys = map T.unpack (HashMap.keys o)
      fields = fieldNames (Proxy :: Proxy a)
      ignoreUnderscored
        | ignoreUnderscoredUnknownFields (Proxy :: Proxy a) = filter (not . isPrefixOf "_")
        | otherwise = id
  _ -> []

data CustomSetupSection = CustomSetupSection {
  customSetupSectionDependencies :: Maybe (List Dependency)
} deriving (Eq, Show, Generic)

instance HasFieldNames CustomSetupSection

instance FromJSON CustomSetupSection where
  parseJSON = genericParseJSON_

data LibrarySection = LibrarySection {
  librarySectionExposed :: Maybe Bool
, librarySectionExposedModules :: Maybe (List String)
, librarySectionOtherModules :: Maybe (List String)
, librarySectionReexportedModules :: Maybe (List String)
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
, commonOptionsCcOptions :: Maybe (List CcOption)
, commonOptionsCSources :: Maybe (List FilePath)
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List ConditionalSection)
, commonOptionsBuildTools :: Maybe (List Dependency)
} deriving (Eq, Show, Generic)

instance HasFieldNames CommonOptions

instance FromJSON CommonOptions where
  parseJSON = genericParseJSON_

data ConditionalSection = ThenElseConditional (CaptureUnknownFields ThenElse) | FlatConditional (CaptureUnknownFields (Section Condition))
  deriving (Eq, Show)

instance FromJSON ConditionalSection where
  parseJSON v
    | hasKey "then" v || hasKey "else" v = ThenElseConditional <$> parseJSON v
    | otherwise = FlatConditional <$> parseJSON v

hasKey :: Text -> Value -> Bool
hasKey key (Object o) = HashMap.member key o
hasKey _ _ = False

newtype Condition = Condition {
  conditionCondition :: String
} deriving (Eq, Show, Generic)

instance FromJSON Condition where
  parseJSON = genericParseJSON_

instance HasFieldNames Condition

data ThenElse = ThenElse {
  _thenElseCondition :: String
, _thenElseThen :: (CaptureUnknownFields (Section Empty))
, _thenElseElse :: (CaptureUnknownFields (Section Empty))
} deriving (Eq, Show, Generic)

instance FromJSON (CaptureUnknownFields ThenElse) where
  parseJSON = captureUnknownFields

instance HasFieldNames ThenElse

instance FromJSON ThenElse where
  parseJSON = genericParseJSON_

data Empty = Empty
  deriving (Eq, Show)

instance FromJSON Empty where
  parseJSON _ = return Empty

instance HasFieldNames Empty where
  fieldNames _ = []

-- From Cabal the library, copied here to avoid a dependency on Cabal.
data BuildType
  = Simple
  | Configure
  | Make
  | Custom
  deriving (Eq, Show, Generic)

instance FromJSON BuildType where
  parseJSON = withText "String" $ \case
    "Simple"    -> return Simple
    "Configure" -> return Configure
    "Make"      -> return Make
    "Custom"    -> return Custom
    _           -> fail "build-type must be one of: Simple, Configure, Make, Custom"

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
, packageConfigBuildType :: Maybe BuildType
, packageConfigLicense :: Maybe String
, packageConfigLicenseFile :: Maybe (List String)
, packageConfigTestedWith :: Maybe String
, packageConfigFlags :: Maybe (Map String (CaptureUnknownFields FlagSection))
, packageConfigExtraSourceFiles :: Maybe (List FilePath)
, packageConfigDataFiles :: Maybe (List FilePath)
, packageConfigGithub :: Maybe Text
, packageConfigGit :: Maybe String
, packageConfigCustomSetup :: Maybe (CaptureUnknownFields CustomSetupSection)
, packageConfigLibrary :: Maybe (CaptureUnknownFields (Section LibrarySection))
, packageConfigExecutables :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigTests :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
, packageConfigBenchmarks :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
} deriving (Eq, Show, Generic)

instance HasFieldNames PackageConfig where
  ignoreUnderscoredUnknownFields _ = True

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

          local :: Parser AddSource
          local = Local <$> o .: "path"

          git :: Parser AddSource
          git = GitRef <$> url <*> ref <*> subdir

          url :: Parser String
          url =
                ((githubBaseUrl ++) <$> o .: "github")
            <|> (o .: "git")
            <|> fail "neither key \"git\" nor key \"github\" present"

          ref :: Parser String
          ref = o .: "ref"

          subdir :: Parser (Maybe FilePath)
          subdir = o .:? "subdir"

data AddSource = GitRef GitUrl GitRef (Maybe FilePath) | Local FilePath
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
, packageBuildType :: BuildType
, packageLicense :: Maybe String
, packageLicenseFile :: [FilePath]
, packageTestedWith :: Maybe String
, packageFlags :: [Flag]
, packageExtraSourceFiles :: [FilePath]
, packageDataFiles :: [FilePath]
, packageSourceRepository :: Maybe SourceRepository
, packageCustomSetup :: Maybe CustomSetup
, packageLibrary :: Maybe (Section Library)
, packageExecutables :: [Section Executable]
, packageTests :: [Section Executable]
, packageBenchmarks :: [Section Executable]
} deriving (Eq, Show)

data CustomSetup = CustomSetup {
  customSetupDependencies :: [Dependency]
} deriving (Eq, Show)

data Library = Library {
  libraryExposed :: Maybe Bool
, libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryReexportedModules :: [String]
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
, sectionCcOptions :: [CcOption]
, sectionCSources :: [FilePath]
, sectionExtraLibDirs :: [FilePath]
, sectionExtraLibraries :: [FilePath]
, sectionIncludeDirs :: [FilePath]
, sectionInstallIncludes :: [FilePath]
, sectionLdOptions :: [LdOption]
, sectionBuildable :: Maybe Bool
, sectionConditionals :: [Conditional]
, sectionBuildTools :: [Dependency]
} deriving (Eq, Show, Functor, Foldable, Traversable)

data Conditional = Conditional {
  conditionalCondition :: String
, conditionalThen :: Section ()
, conditionalElse :: Maybe (Section ())
} deriving (Eq, Show)

instance HasFieldNames a => HasFieldNames (Section a) where
  fieldNames Proxy = fieldNames (Proxy :: Proxy a) ++ fieldNames (Proxy :: Proxy CommonOptions)
  ignoreUnderscoredUnknownFields _ = ignoreUnderscoredUnknownFields (Proxy :: Proxy a)

data FlagSection = FlagSection {
  _flagSectionDescription :: Maybe String
, _flagSectionManual :: Bool
, _flagSectionDefault :: Bool
} deriving (Eq, Show, Generic)

instance HasFieldNames FlagSection

instance FromJSON FlagSection where
  parseJSON = genericParseJSON_

data Flag = Flag {
  flagName :: String
, flagDescription :: Maybe String
, flagManual :: Bool
, flagDefault :: Bool
} deriving (Eq, Show)

toFlag :: (String, FlagSection) -> Flag
toFlag (name, FlagSection description manual def) = Flag name description manual def

data SourceRepository = SourceRepository {
  sourceRepositoryUrl :: String
, sourceRepositorySubdir :: Maybe String
} deriving (Eq, Show)

mkPackage :: FilePath -> (CaptureUnknownFields (Section PackageConfig)) -> IO ([String], Package)
mkPackage dir (CaptureUnknownFields unknownFields globalOptions@Section{sectionData = PackageConfig{..}}) = do
  let
    nameWarnings :: [String]
    name :: String
    (nameWarnings, name) = maybe (["Package name not specified, inferred " ++ show inferredName], inferredName) ((,) []) packageConfigName
      where inferredName = takeBaseName dir

    mCustomSetup :: Maybe CustomSetup
    mCustomSetup = toCustomSetup <$> mCustomSetupSection

  libraryResult <- mapM (toLibrary dir name globalOptions) mLibrarySection
  let
    mLibrary :: Maybe (Section Library)
    mLibrary = fmap snd libraryResult

    libraryWarnings :: [String]
    libraryWarnings = maybe [] fst libraryResult

  (executablesWarnings, executables) <- toExecutables dir globalOptions (map (fmap captureUnknownFieldsValue) executableSections)
  (testsWarnings, tests) <- toExecutables dir globalOptions (map (fmap captureUnknownFieldsValue) testsSections)
  (benchmarksWarnings, benchmarks) <- toExecutables dir globalOptions  (map (fmap captureUnknownFieldsValue) benchmarkSections)

  licenseFileExists <- doesFileExist (dir </> "LICENSE")

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist . (dir </>)) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs executables
    ++ concatMap sectionSourceDirs tests
    ++ concatMap sectionSourceDirs benchmarks
    )

  (extraSourceFilesWarnings, extraSourceFiles) <-
    expandGlobs "extra-source-files" dir (fromMaybeList packageConfigExtraSourceFiles)

  (dataFilesWarnings, dataFiles) <-
    expandGlobs "data-files" dir (fromMaybeList packageConfigDataFiles)

  let defaultBuildType :: BuildType
      defaultBuildType = maybe Simple (const Custom) mCustomSetup

      configLicenseFiles :: Maybe (List String)
      configLicenseFiles = packageConfigLicenseFile <|> do
        guard licenseFileExists
        Just (List ["LICENSE"])

      pkg = Package {
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
      , packageBuildType = fromMaybe defaultBuildType packageConfigBuildType
      , packageLicense = packageConfigLicense
      , packageLicenseFile = fromMaybeList configLicenseFiles
      , packageTestedWith = packageConfigTestedWith
      , packageFlags = flags
      , packageExtraSourceFiles = extraSourceFiles
      , packageDataFiles = dataFiles
      , packageSourceRepository = sourceRepository
      , packageCustomSetup = mCustomSetup
      , packageLibrary = mLibrary
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ nameWarnings
        ++ flagWarnings
        ++ maybe [] (formatUnknownFields "custom-setup section") (captureUnknownFieldsFields <$> packageConfigCustomSetup)
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary)
        ++ formatUnknownSectionFields "executable" executableSections
        ++ formatUnknownSectionFields "test" testsSections
        ++ formatMissingSourceDirs missingSourceDirs
        ++ libraryWarnings
        ++ executablesWarnings
        ++ testsWarnings
        ++ benchmarksWarnings
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

    (flagWarnings, flags) = (concatMap formatUnknownFlagFields xs, map (toFlag . fmap captureUnknownFieldsValue) xs)
      where
        xs :: [(String, CaptureUnknownFields FlagSection)]
        xs = toList packageConfigFlags

        formatUnknownFlagFields :: (String, CaptureUnknownFields a) -> [String]
        formatUnknownFlagFields (name, fields) = map f (captureUnknownFieldsFields fields)
          where f field = "Ignoring unknown field " ++ show field ++ " for flag " ++ show name

    toList :: Maybe (Map String a) -> [(String, a)]
    toList = Map.toList . fromMaybe mempty

    mCustomSetupSection :: Maybe CustomSetupSection
    mCustomSetupSection = captureUnknownFieldsValue <$> packageConfigCustomSetup

    mLibrarySection :: Maybe (Section LibrarySection)
    mLibrarySection = captureUnknownFieldsValue <$> packageConfigLibrary

    formatUnknownFields :: String -> [FieldName] -> [String]
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
    sourceRepository = github <|> (`SourceRepository` Nothing) <$> packageConfigGit

    github :: Maybe SourceRepository
    github = parseGithub <$> packageConfigGithub
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
        fromGithub = (++ "#readme") . sourceRepositoryUrl <$> github

    bugReports :: Maybe String
    bugReports = case packageConfigBugReports of
      Just Nothing -> Nothing
      _ -> join packageConfigBugReports <|> fromGithub
      where
        fromGithub = (++ "/issues") . sourceRepositoryUrl <$> github

expandCSources :: FilePath -> Section a -> IO ([String], Section a)
expandCSources dir sect@Section{..} = do
  (warnings, files) <- expandGlobs "c-sources" dir sectionCSources
  return (warnings, sect {sectionCSources = files})

toCustomSetup :: CustomSetupSection -> CustomSetup
toCustomSetup CustomSetupSection{..} = CustomSetup
  { customSetupDependencies = fromMaybeList customSetupSectionDependencies }

toLibrary :: FilePath -> String -> Section global -> Section LibrarySection -> IO ([String], Section Library)
toLibrary dir name globalOptions library = traverse fromLibrarySection sect >>= expandCSources dir
  where
    sect :: Section LibrarySection
    sect = mergeSections globalOptions library

    sourceDirs :: [FilePath]
    sourceDirs = sectionSourceDirs sect

    fromLibrarySection :: LibrarySection -> IO Library
    fromLibrarySection LibrarySection{..} = do
      modules <- concat <$> mapM (getModules dir) sourceDirs
      let (exposedModules, otherModules) = determineModules name modules librarySectionExposedModules librarySectionOtherModules
          reexportedModules = fromMaybeList librarySectionReexportedModules
      return (Library librarySectionExposed exposedModules otherModules reexportedModules)

toExecutables :: FilePath -> Section global -> [(String, Section ExecutableSection)] -> IO ([String], [Section Executable])
toExecutables dir globalOptions executables = do
  result <- mapM toExecutable sections >>= mapM (expandCSources dir)
  let (warnings, xs) = unzip result
  return (concat warnings, xs)
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
  = Section {
    sectionData = sectionData options
  , sectionSourceDirs = sectionSourceDirs globalOptions ++ sectionSourceDirs options
  , sectionDefaultExtensions = sectionDefaultExtensions globalOptions ++ sectionDefaultExtensions options
  , sectionOtherExtensions = sectionOtherExtensions globalOptions ++ sectionOtherExtensions options
  , sectionGhcOptions = sectionGhcOptions globalOptions ++ sectionGhcOptions options
  , sectionGhcProfOptions = sectionGhcProfOptions globalOptions ++ sectionGhcProfOptions options
  , sectionCppOptions = sectionCppOptions globalOptions ++ sectionCppOptions options
  , sectionCcOptions = sectionCcOptions globalOptions ++ sectionCcOptions options
  , sectionCSources = sectionCSources globalOptions ++ sectionCSources options
  , sectionExtraLibDirs = sectionExtraLibDirs globalOptions ++ sectionExtraLibDirs options
  , sectionExtraLibraries = sectionExtraLibraries globalOptions ++ sectionExtraLibraries options
  , sectionIncludeDirs = sectionIncludeDirs globalOptions ++ sectionIncludeDirs options
  , sectionInstallIncludes = sectionInstallIncludes globalOptions ++ sectionInstallIncludes options
  , sectionLdOptions = sectionLdOptions globalOptions ++ sectionLdOptions options
  , sectionBuildable = sectionBuildable options <|> sectionBuildable globalOptions
  , sectionDependencies = sectionDependencies globalOptions ++ sectionDependencies options
  , sectionConditionals = sectionConditionals globalOptions ++ sectionConditionals options
  , sectionBuildTools = sectionBuildTools globalOptions ++ sectionBuildTools options
  }

toSection :: a -> CommonOptions -> ([FieldName], Section a)
toSection a CommonOptions{..}
  = ( concat unknownFields
    , Section {
        sectionData = a
      , sectionSourceDirs = fromMaybeList commonOptionsSourceDirs
      , sectionDefaultExtensions = fromMaybeList commonOptionsDefaultExtensions
      , sectionOtherExtensions = fromMaybeList commonOptionsOtherExtensions
      , sectionGhcOptions = fromMaybeList commonOptionsGhcOptions
      , sectionGhcProfOptions = fromMaybeList commonOptionsGhcProfOptions
      , sectionCppOptions = fromMaybeList commonOptionsCppOptions
      , sectionCcOptions = fromMaybeList commonOptionsCcOptions
      , sectionCSources = fromMaybeList commonOptionsCSources
      , sectionExtraLibDirs = fromMaybeList commonOptionsExtraLibDirs
      , sectionExtraLibraries = fromMaybeList commonOptionsExtraLibraries
      , sectionIncludeDirs = fromMaybeList commonOptionsIncludeDirs
      , sectionInstallIncludes = fromMaybeList commonOptionsInstallIncludes
      , sectionLdOptions = fromMaybeList commonOptionsLdOptions
      , sectionBuildable = commonOptionsBuildable
      , sectionDependencies = fromMaybeList commonOptionsDependencies
      , sectionConditionals = conditionals
      , sectionBuildTools = fromMaybeList commonOptionsBuildTools
      }
    )
  where
    (unknownFields, conditionals) = unzip (map toConditional $ fromMaybeList commonOptionsWhen)

toConditional :: ConditionalSection -> ([FieldName], Conditional)
toConditional x = case x of
  ThenElseConditional (CaptureUnknownFields fields (ThenElse condition (CaptureUnknownFields fieldsThen then_) (CaptureUnknownFields fieldsElse else_))) ->
      (fields ++ fieldsThen ++ fieldsElse, Conditional condition (() <$ then_) (Just (() <$ else_)))
  FlatConditional (CaptureUnknownFields fields sect) -> (fields, Conditional (conditionCondition $ sectionData sect) (() <$ sect) Nothing)

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
