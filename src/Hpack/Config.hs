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
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Hpack.Config (
  packageConfig
, readPackageConfig
, renamePackage
, packageDependencies
, package
, section
, Package(..)
, Dependencies(..)
, DependencyVersion(..)
, SourceDependency(..)
, GitRef
, GitUrl
, GhcOption
, CustomSetup(..)
, Section(..)
, Library(..)
, Executable(..)
, Conditional(..)
, Flag(..)
, SourceRepository(..)
#ifdef TEST
, toSection
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
import           Data.Bifunctor
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HashMap
import           Data.List.Compat (nub, (\\), sortBy, isPrefixOf)
import           Data.Maybe
import           Data.Monoid hiding (Product)
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
import           Hpack.Dependency

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
  , packageInternalLibraries = mempty
  , packageExecutables = mempty
  , packageTests = mempty
  , packageBenchmarks = mempty
  }

renamePackage :: String -> Package -> Package
renamePackage name p@Package{..} = p {
    packageName = name
  , packageExecutables = fmap (renameDependencies packageName name) packageExecutables
  , packageTests = fmap (renameDependencies packageName name) packageTests
  , packageBenchmarks = fmap (renameDependencies packageName name) packageBenchmarks
  }

renameDependencies :: String -> String -> Section a -> Section a
renameDependencies old new sect@Section{..} = sect {sectionDependencies = (Dependencies . Map.fromList . map rename . Map.toList . unDependencies) sectionDependencies, sectionConditionals = map renameConditional sectionConditionals}
  where
    rename dep@(name, version)
      | name == old = (new, version)
      | otherwise = dep

    renameConditional :: Conditional (Section a) -> Conditional (Section a)
    renameConditional (Conditional condition then_ else_) = Conditional condition (renameDependencies old new then_) (renameDependencies old new <$> else_)

packageDependencies :: Package -> [(String, DependencyVersion)]
packageDependencies Package{..} = nub . sortBy (comparing (lexicographically . fst)) $
     (concatMap deps packageExecutables)
  ++ (concatMap deps packageTests)
  ++ (concatMap deps packageBenchmarks)
  ++ maybe [] deps packageLibrary
  where
    deps xs = [(name, version) | (name, version) <- (Map.toList . unDependencies . sectionDependencies) xs]

section :: a -> Section a
section a = Section a [] mempty [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] Nothing [] mempty

packageConfig :: FilePath
packageConfig = "package.yaml"

#if MIN_VERSION_aeson(1,0,0)
genericParseJSON_ :: forall a d m. (GFromJSON Zero (Rep a), HasTypeName a d m) => Value -> Parser a
#else
genericParseJSON_ :: forall a d m. (GFromJSON (Rep a), HasTypeName a d m) => Value -> Parser a
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

  default fieldNames :: (HasTypeName a d m, Selectors (Rep a)) => Proxy a -> [String]
  fieldNames proxy = map (hyphenize $ typeName proxy) (selectors proxy)

  ignoreUnderscoredUnknownFields :: Proxy a -> Bool
  ignoreUnderscoredUnknownFields _ = False

data CaptureUnknownFields a = CaptureUnknownFields {
  captureUnknownFieldsFields :: [FieldName]
, captureUnknownFieldsValue :: a
} deriving (Eq, Show, Functor, Generic)

captureUnknownFields :: forall a. (HasFieldNames a, FromJSON a) => Value -> Parser (CaptureUnknownFields a)
captureUnknownFields v = CaptureUnknownFields unknown <$> parseJSON v
  where
    unknown = getUnknownFields v (Proxy :: Proxy a)

instance (HasFieldNames a, FromJSON a) => FromJSON (CaptureUnknownFields a) where
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
  customSetupSectionDependencies :: Maybe Dependencies
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

emptyLibrarySection :: LibrarySection
emptyLibrarySection = LibrarySection Nothing Nothing Nothing Nothing

instance HasFieldNames LibrarySection

instance FromJSON LibrarySection where
  parseJSON = genericParseJSON_

data ExecutableSection = ExecutableSection {
  executableSectionMain :: Maybe FilePath
, executableSectionOtherModules :: Maybe (List String)
} deriving (Eq, Show, Generic)

emptyExecutableSection :: ExecutableSection
emptyExecutableSection = ExecutableSection Nothing Nothing

instance HasFieldNames ExecutableSection

instance FromJSON ExecutableSection where
  parseJSON = genericParseJSON_

data CommonOptions a = CommonOptions {
  commonOptionsSourceDirs :: Maybe (List FilePath)
, commonOptionsDependencies :: Maybe Dependencies
, commonOptionsDefaultExtensions :: Maybe (List String)
, commonOptionsOtherExtensions :: Maybe (List String)
, commonOptionsGhcOptions :: Maybe (List GhcOption)
, commonOptionsGhcProfOptions :: Maybe (List GhcProfOption)
, commonOptionsGhcjsOptions :: Maybe (List GhcjsOption)
, commonOptionsCppOptions :: Maybe (List CppOption)
, commonOptionsCcOptions :: Maybe (List CcOption)
, commonOptionsCSources :: Maybe (List FilePath)
, commonOptionsJsSources :: Maybe (List FilePath)
, commonOptionsExtraLibDirs :: Maybe (List FilePath)
, commonOptionsExtraLibraries :: Maybe (List FilePath)
, commonOptionsExtraFrameworksDirs :: Maybe (List FilePath)
, commonOptionsFrameworks :: Maybe (List String)
, commonOptionsIncludeDirs :: Maybe (List FilePath)
, commonOptionsInstallIncludes :: Maybe (List FilePath)
, commonOptionsLdOptions :: Maybe (List LdOption)
, commonOptionsBuildable :: Maybe Bool
, commonOptionsWhen :: Maybe (List (ConditionalSection a))
, commonOptionsBuildTools :: Maybe Dependencies
} deriving (Eq, Show, Generic)

instance HasFieldNames (CommonOptions a)

instance (FromJSON a, HasFieldNames a) => FromJSON (CommonOptions a) where
  parseJSON = genericParseJSON_

type WithCommonOptions a = Product (CommonOptions a) a

data Product a b = Product a b
  deriving (Eq, Show)

instance Bifunctor Product where
  bimap fa fb (Product a b) = Product (fa a) (fb b)

instance (FromJSON a, FromJSON b) => FromJSON (Product a b) where
  parseJSON value = Product <$> parseJSON value <*> parseJSON value

instance (HasFieldNames a, HasFieldNames b) => HasFieldNames (Product a b) where
  fieldNames Proxy =
       fieldNames (Proxy :: Proxy a)
    ++ fieldNames (Proxy :: Proxy b)
  ignoreUnderscoredUnknownFields Proxy =
       ignoreUnderscoredUnknownFields (Proxy :: Proxy a)
    || ignoreUnderscoredUnknownFields (Proxy :: Proxy b)

data ConditionalSection a =
    ThenElseConditional (CaptureUnknownFields (ThenElse a))
  | FlatConditional (CaptureUnknownFields (Product (WithCommonOptions a) Condition))
  deriving (Eq, Show)

instance (FromJSON a, HasFieldNames a) => FromJSON (ConditionalSection a) where
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

data ThenElse a = ThenElse {
  _thenElseCondition :: String
, _thenElseThen :: CaptureUnknownFields (WithCommonOptions a)
, _thenElseElse :: CaptureUnknownFields (WithCommonOptions a)
} deriving (Eq, Show, Generic)

instance HasFieldNames (ThenElse a)

instance (FromJSON a, HasFieldNames a) => FromJSON (ThenElse a) where
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

type ExecutableConfig = CaptureUnknownFields (WithCommonOptions ExecutableSection)

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
, packageConfigLibrary :: Maybe (CaptureUnknownFields (WithCommonOptions LibrarySection))
, packageConfigInternalLibraries :: Maybe (Map String (CaptureUnknownFields (WithCommonOptions LibrarySection)))
, packageConfigExecutable :: Maybe ExecutableConfig
, packageConfigExecutables :: Maybe (Map String ExecutableConfig)
, packageConfigTests :: Maybe (Map String ExecutableConfig)
, packageConfigBenchmarks :: Maybe (Map String ExecutableConfig)
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
      Right <$> toPackage dir config

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
, packageInternalLibraries :: Map String (Section Library)
, packageExecutables :: Map String (Section Executable)
, packageTests :: Map String (Section Executable)
, packageBenchmarks :: Map String (Section Executable)
} deriving (Eq, Show)

data CustomSetup = CustomSetup {
  customSetupDependencies :: Dependencies
} deriving (Eq, Show)

data Library = Library {
  libraryExposed :: Maybe Bool
, libraryExposedModules :: [String]
, libraryOtherModules :: [String]
, libraryReexportedModules :: [String]
} deriving (Eq, Show)

data Executable = Executable {
  executableMain :: Maybe FilePath
, executableOtherModules :: [String]
} deriving (Eq, Show)

data Section a = Section {
  sectionData :: a
, sectionSourceDirs :: [FilePath]
, sectionDependencies :: Dependencies
, sectionDefaultExtensions :: [String]
, sectionOtherExtensions :: [String]
, sectionGhcOptions :: [GhcOption]
, sectionGhcProfOptions :: [GhcProfOption]
, sectionGhcjsOptions :: [GhcjsOption]
, sectionCppOptions :: [CppOption]
, sectionCcOptions :: [CcOption]
, sectionCSources :: [FilePath]
, sectionJsSources :: [FilePath]
, sectionExtraLibDirs :: [FilePath]
, sectionExtraLibraries :: [FilePath]
, sectionExtraFrameworksDirs :: [FilePath]
, sectionFrameworks :: [FilePath]
, sectionIncludeDirs :: [FilePath]
, sectionInstallIncludes :: [FilePath]
, sectionLdOptions :: [LdOption]
, sectionBuildable :: Maybe Bool
, sectionConditionals :: [Conditional (Section a)]
, sectionBuildTools :: Dependencies
} deriving (Eq, Show, Functor, Foldable, Traversable)

data Conditional a = Conditional {
  conditionalCondition :: String
, conditionalThen :: a
, conditionalElse :: Maybe a
} deriving (Eq, Show, Functor, Foldable, Traversable)

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

toSection :: CaptureUnknownFields (WithCommonOptions a) -> CaptureUnknownFields (Section a)
toSection (CaptureUnknownFields unknownSectionFields (Product common a)) = case toSection_ a common of
  (unknownFields, sect) -> CaptureUnknownFields (unknownSectionFields ++ unknownFields) sect

toEmptySection :: CaptureUnknownFields (Product (CommonOptions Empty) a) -> CaptureUnknownFields (Section Empty, a)
toEmptySection (CaptureUnknownFields unknownSectionFields (Product common a)) = case toSection_ Empty common of
  (unknownFields, sect) -> CaptureUnknownFields (unknownSectionFields ++ unknownFields) (sect, a)

toPackage :: FilePath -> (CaptureUnknownFields (Product (CommonOptions Empty) PackageConfig)) -> IO ([String], Package)
toPackage dir (toEmptySection -> CaptureUnknownFields unknownFields (globalOptions, PackageConfig{..})) = do
  libraryResult <- mapM (toLibrary dir packageName_ globalOptions) mLibrarySection
  let
    packageConfigExecutables_ :: Maybe (Map String (CaptureUnknownFields (Section ExecutableSection)))
    packageConfigExecutables_ = fmap toSection <$> packageConfigExecutables

    packageConfigExecutable_ :: Maybe (CaptureUnknownFields (Section ExecutableSection))
    packageConfigExecutable_ = toSection <$> packageConfigExecutable

    executableWarnings :: [String]
    executableSections :: [(String, Section ExecutableSection)]
    (executableWarnings, executableSections) = (warnings, map (fmap captureUnknownFieldsValue) sections)
      where
        sections = case (packageConfigExecutable_, packageConfigExecutables_) of
          (Nothing, Nothing) -> []
          (Just executable, _) -> [(packageName_, executable)]
          (Nothing, Just executables) -> Map.toList executables

        warnings = ignoringExecutablesWarning ++ unknownFieldWarnings
        ignoringExecutablesWarning = case (packageConfigExecutable_, packageConfigExecutables_) of
          (Just _, Just _) -> ["Ignoring field \"executables\" in favor of \"executable\""]
          _ -> []
        unknownFieldWarnings = formatUnknownSectionFields (isJust packageConfigExecutables_) "executable" sections

    mLibrary :: Maybe (Section Library)
    mLibrary = fmap snd libraryResult

    libraryWarnings :: [String]
    libraryWarnings = maybe [] fst libraryResult

  (internalLibrariesWarnings, internalLibraries) <- toInternalLibraries dir packageName_ globalOptions (map (fmap captureUnknownFieldsValue) internalLibrariesSections)
  (executablesWarnings, executables) <- toExecutables dir packageName_ globalOptions executableSections
  (testsWarnings, tests) <- toExecutables dir packageName_ globalOptions (map (fmap captureUnknownFieldsValue) testsSections)
  (benchmarksWarnings, benchmarks) <- toExecutables dir packageName_ globalOptions  (map (fmap captureUnknownFieldsValue) benchmarkSections)

  licenseFileExists <- doesFileExist (dir </> "LICENSE")

  missingSourceDirs <- nub . sort <$> filterM (fmap not <$> doesDirectoryExist . (dir </>)) (
       maybe [] sectionSourceDirs mLibrary
    ++ concatMap sectionSourceDirs internalLibraries
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
        packageName = packageName_
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
      , packageInternalLibraries = internalLibraries
      , packageExecutables = executables
      , packageTests = tests
      , packageBenchmarks = benchmarks
      }

      warnings =
           formatUnknownFields "package description" unknownFields
        ++ nameWarnings
        ++ flagWarnings
        ++ maybe [] (formatUnknownFields "custom-setup section") (captureUnknownFieldsFields <$> packageConfigCustomSetup)
        ++ maybe [] (formatUnknownFields "library section") (captureUnknownFieldsFields <$> packageConfigLibrary_)
        ++ formatUnknownSectionFields True "internal-libraries" internalLibrariesSections
        ++ formatUnknownSectionFields True "test" testsSections
        ++ formatUnknownSectionFields True "benchmark" benchmarkSections
        ++ formatMissingSourceDirs missingSourceDirs
        ++ libraryWarnings
        ++ internalLibrariesWarnings
        ++ executableWarnings
        ++ executablesWarnings
        ++ testsWarnings
        ++ benchmarksWarnings
        ++ extraSourceFilesWarnings
        ++ dataFilesWarnings

  return (warnings, pkg)
  where
    nameWarnings :: [String]
    packageName_ :: String
    (nameWarnings, packageName_) = case packageConfigName of
      Nothing -> let inferredName = takeBaseName dir in
        (["Package name not specified, inferred " ++ show inferredName], inferredName)
      Just n -> ([], n)

    mCustomSetup :: Maybe CustomSetup
    mCustomSetup = toCustomSetup <$> mCustomSetupSection

    packageConfigLibrary_ = toSection <$> packageConfigLibrary

    internalLibrariesSections :: [(String, CaptureUnknownFields (Section LibrarySection))]
    internalLibrariesSections = map (fmap toSection) $ toList packageConfigInternalLibraries

    testsSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    testsSections = map (fmap toSection) $ toList packageConfigTests

    benchmarkSections :: [(String, CaptureUnknownFields (Section ExecutableSection))]
    benchmarkSections = map (fmap toSection) $ toList packageConfigBenchmarks

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
    mLibrarySection = captureUnknownFieldsValue <$> packageConfigLibrary_

    formatUnknownFields :: String -> [FieldName] -> [String]
    formatUnknownFields name = map f . sort
      where
        f field = "Ignoring unknown field " ++ show field ++ " in " ++ name

    formatUnknownSectionFields :: Bool -> String -> [(String, CaptureUnknownFields a)] -> [String]
    formatUnknownSectionFields showSect sectionType = concatMap f . map (fmap captureUnknownFieldsFields)
      where
        f :: (String, [String]) -> [String]
        f (sect, fields) = formatUnknownFields
          (sectionType ++ " section" ++ if showSect then " " ++ show sect else "")
          fields

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

expandJsSources :: FilePath -> Section a -> IO ([String], Section a)
expandJsSources dir sect@Section{..} = do
  (warnings, files) <- expandGlobs "js-sources" dir sectionJsSources
  return (warnings, sect {sectionJsSources = files})

expandForeignSources :: FilePath -> Section a -> IO ([String], Section a)
expandForeignSources dir sect = do
  (cWarnings, sect_) <- expandCSources dir sect
  (jsWarnings, sect__) <- expandJsSources dir sect_
  return (cWarnings ++ jsWarnings, sect__)

toCustomSetup :: CustomSetupSection -> CustomSetup
toCustomSetup CustomSetupSection{..} = CustomSetup
  { customSetupDependencies = fromMaybe mempty customSetupSectionDependencies }

traverseSection :: Applicative m => (Section a -> m b) -> Section a -> m (Section b)
traverseSection f = biTraverseSection f f

biTraverseSection :: Applicative m => (Section a -> m b) -> (Section a -> m b) -> Section a -> m (Section b)
biTraverseSection fData fConditionals sect =
  update <$> fData sect <*> traverse (traverse $ traverseSection fConditionals) (sectionConditionals sect)
  where
    update x xs = sect{sectionData = x, sectionConditionals = xs}

toLibrary :: FilePath -> String -> Section global -> Section LibrarySection -> IO ([String], Section Library)
toLibrary dir name globalOptions library =
  biTraverseSection fromLibrarySection fromLibrarySectionInConditional sect >>= expandForeignSources dir
  where
    sect :: Section LibrarySection
    sect = mergeSections emptyLibrarySection globalOptions library

    fromLibrarySection :: Section LibrarySection -> IO Library
    fromLibrarySection Section{sectionData = LibrarySection{..}, sectionSourceDirs = sourceDirs} = do
      modules <- concat <$> mapM (getModules dir) sourceDirs
      let (exposedModules, otherModules) = determineModules name modules librarySectionExposedModules librarySectionOtherModules
          reexportedModules = fromMaybeList librarySectionReexportedModules
      return (Library librarySectionExposed exposedModules otherModules reexportedModules)

    fromLibrarySectionInConditional :: Section LibrarySection -> IO Library
    fromLibrarySectionInConditional = return . fromLibrarySectionPlain . sectionData

    fromLibrarySectionPlain :: LibrarySection -> Library
    fromLibrarySectionPlain LibrarySection{..} = Library {
        libraryExposed = librarySectionExposed
      , libraryExposedModules = fromMaybeList librarySectionExposedModules
      , libraryOtherModules = fromMaybeList librarySectionOtherModules
      , libraryReexportedModules = fromMaybeList librarySectionReexportedModules
      }

nubOtherModules :: Section Executable -> Section Executable
nubOtherModules = mapSectionAcc f []
  where
    f :: [String] -> Executable -> ([String], Executable)
    f outerOtherModules executable = (
        outerOtherModules ++ otherModules
      , executable {executableOtherModules = otherModules}
      )
      where
        otherModules = executableOtherModules executable \\ outerOtherModules

mapSectionAcc :: (acc -> a -> (acc, b)) -> acc -> Section a -> Section b
mapSectionAcc f = go
  where
    go acc0 sect@Section{..} = let (acc, b) = f acc0 sectionData in sect {
        sectionData = b
      , sectionConditionals = map (go acc <$>) sectionConditionals
      }

toInternalLibraries :: FilePath -> String -> Section global -> [(String, Section LibrarySection)] -> IO ([String], Map String (Section Library))
toInternalLibraries dir packageName_ globalOptions internalLibraries = do
  results <- mapM (traverse $ toLibrary dir packageName_ globalOptions) internalLibraries
  let warnings = map (fst . snd) results
      internalLibraries' = map (fmap snd) results
  return (concat warnings, Map.fromList internalLibraries')

toExecutables :: FilePath -> String -> Section global -> [(String, Section ExecutableSection)] -> IO ([String], Map String (Section Executable))
toExecutables dir packageName_ globalOptions executables = do
  result <- mapM toExecutable sections >>= mapM (expandForeignSources dir . nubOtherModules)
  let (warnings, xs) = unzip result
  return (concat warnings, Map.fromList $ zip names xs)
  where
    namedSections :: [(String, Section ExecutableSection)]
    namedSections = map (fmap $ mergeSections emptyExecutableSection globalOptions) executables

    names = map fst namedSections
    sections = map snd namedSections

    toExecutable :: Section ExecutableSection -> IO (Section Executable)
    toExecutable sect@Section{..} = do
      (executable, ghcOptions) <- fromExecutableSection sectionData
      conditionals <- mapM (traverse toExecutable) sectionConditionals
      return sect {
          sectionData = executable
        , sectionGhcOptions = sectionGhcOptions ++ ghcOptions
        , sectionConditionals = conditionals
        }
      where
        fromExecutableSection :: ExecutableSection -> IO (Executable, [GhcOption])
        fromExecutableSection ExecutableSection{..} = do
          modules <- maybe inferModules (return . fromList) executableSectionOtherModules
          return (Executable mainSrcFile modules, ghcOptions)
          where
            inferModules :: IO [String]
            inferModules
              | null sectionSourceDirs = return []
              | otherwise = filterMain . (++ [pathsModule]) . concat <$> mapM (getModules dir) sectionSourceDirs

            pathsModule = pathsModuleFromPackageName packageName_

            filterMain :: [String] -> [String]
            filterMain = maybe id (maybe id (filter . (/=)) . toModule . splitDirectories) executableSectionMain

            (mainSrcFile, ghcOptions) = maybe (Nothing, []) (first Just . parseMain) executableSectionMain

mergeSections :: a -> Section global -> Section a -> Section a
mergeSections a globalOptions options
  = Section {
    sectionData = sectionData options
  , sectionSourceDirs = sectionSourceDirs globalOptions ++ sectionSourceDirs options
  , sectionDefaultExtensions = sectionDefaultExtensions globalOptions ++ sectionDefaultExtensions options
  , sectionOtherExtensions = sectionOtherExtensions globalOptions ++ sectionOtherExtensions options
  , sectionGhcOptions = sectionGhcOptions globalOptions ++ sectionGhcOptions options
  , sectionGhcProfOptions = sectionGhcProfOptions globalOptions ++ sectionGhcProfOptions options
  , sectionGhcjsOptions = sectionGhcjsOptions globalOptions ++ sectionGhcjsOptions options
  , sectionCppOptions = sectionCppOptions globalOptions ++ sectionCppOptions options
  , sectionCcOptions = sectionCcOptions globalOptions ++ sectionCcOptions options
  , sectionCSources = sectionCSources globalOptions ++ sectionCSources options
  , sectionJsSources = sectionJsSources globalOptions ++ sectionJsSources options
  , sectionExtraLibDirs = sectionExtraLibDirs globalOptions ++ sectionExtraLibDirs options
  , sectionExtraLibraries = sectionExtraLibraries globalOptions ++ sectionExtraLibraries options
  , sectionExtraFrameworksDirs = sectionExtraFrameworksDirs globalOptions ++ sectionExtraFrameworksDirs options
  , sectionFrameworks = sectionFrameworks globalOptions ++ sectionFrameworks options
  , sectionIncludeDirs = sectionIncludeDirs globalOptions ++ sectionIncludeDirs options
  , sectionInstallIncludes = sectionInstallIncludes globalOptions ++ sectionInstallIncludes options
  , sectionLdOptions = sectionLdOptions globalOptions ++ sectionLdOptions options
  , sectionBuildable = sectionBuildable options <|> sectionBuildable globalOptions
  , sectionDependencies = sectionDependencies options <> sectionDependencies globalOptions
  , sectionConditionals = map (fmap (a <$)) (sectionConditionals globalOptions) ++ sectionConditionals options
  , sectionBuildTools = sectionBuildTools options <> sectionBuildTools globalOptions
  }

toSection_ :: a -> CommonOptions a -> ([FieldName], Section a)
toSection_ a CommonOptions{..}
  = ( concat unknownFields
    , Section {
        sectionData = a
      , sectionSourceDirs = fromMaybeList commonOptionsSourceDirs
      , sectionDefaultExtensions = fromMaybeList commonOptionsDefaultExtensions
      , sectionOtherExtensions = fromMaybeList commonOptionsOtherExtensions
      , sectionGhcOptions = fromMaybeList commonOptionsGhcOptions
      , sectionGhcProfOptions = fromMaybeList commonOptionsGhcProfOptions
      , sectionGhcjsOptions = fromMaybeList commonOptionsGhcjsOptions
      , sectionCppOptions = fromMaybeList commonOptionsCppOptions
      , sectionCcOptions = fromMaybeList commonOptionsCcOptions
      , sectionCSources = fromMaybeList commonOptionsCSources
      , sectionJsSources = fromMaybeList commonOptionsJsSources
      , sectionExtraLibDirs = fromMaybeList commonOptionsExtraLibDirs
      , sectionExtraLibraries = fromMaybeList commonOptionsExtraLibraries
      , sectionExtraFrameworksDirs = fromMaybeList commonOptionsExtraFrameworksDirs
      , sectionFrameworks = fromMaybeList commonOptionsFrameworks
      , sectionIncludeDirs = fromMaybeList commonOptionsIncludeDirs
      , sectionInstallIncludes = fromMaybeList commonOptionsInstallIncludes
      , sectionLdOptions = fromMaybeList commonOptionsLdOptions
      , sectionBuildable = commonOptionsBuildable
      , sectionDependencies = fromMaybe mempty commonOptionsDependencies
      , sectionConditionals = conditionals
      , sectionBuildTools = fromMaybe mempty commonOptionsBuildTools
      }
    )
  where
    (unknownFields, conditionals) = unzip (map toConditional $ fromMaybeList commonOptionsWhen)

toConditional :: ConditionalSection a -> ([FieldName], Conditional (Section a))
toConditional x = case x of
  ThenElseConditional (CaptureUnknownFields fields (ThenElse condition (toSection -> CaptureUnknownFields fieldsThen then_) (toSection -> CaptureUnknownFields fieldsElse else_))) ->
    (fields ++ fieldsThen ++ fieldsElse, Conditional condition then_ (Just else_))

  FlatConditional (CaptureUnknownFields unknownSectionFields (Product (Product common a) c)) -> case toSection_ a common of
    (unknownFields, sect) -> (unknownSectionFields ++ unknownFields, Conditional (conditionCondition c) sect Nothing)

pathsModuleFromPackageName :: String -> String
pathsModuleFromPackageName name = "Paths_" ++ map f name
  where
    f '-' = '_'
    f x = x

determineModules :: String -> [String] -> Maybe (List String) -> Maybe (List String) -> ([String], [String])
determineModules name modules mExposedModules mOtherModules = case (mExposedModules, mOtherModules) of
  (Nothing, Nothing) -> (modules, [pathsModuleFromPackageName name])
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
      removeSetup src . toModules <$> getModuleFilesRecursive src
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
